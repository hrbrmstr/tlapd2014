# MIT License - Copyright (c) 2014 Bob Rudis (@hrbrmstr)

# 0. Make sure to source all the library calls and the following functions ##########
#    before trying any of the marked code sections

library(stringr)
library(pbapply)
library(httr)
library(rvest)
library(XML)
library(jsonlite)
library(data.table)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(gtable)
library(gridExtra)
library(reshape2)
library(scales)
library(biOps) # https://code.google.com/p/rimagebook/wiki/biOpsInstallationEn
library(raster)

# You should do a quick walk through of the accompanying blog post:
# - http://datadrivensecurity.info/blog/2014/09/19/do-you-want-to-steal-a-snowman.html
# before cranking through the code here.

# NOTE that if you use RStudio, the context popup will allow jumping to marked
# sections of code. This script was NOT meant to run all at one. Read and
# execute each section you want to explore.

scrapeMovieData <- function() {

  # get all the Mondays (which is when torrentfreak does their top 10 post)
  # they seem to have started on March 3rd and the URL format varies slightly

  dates <- seq.Date(as.Date("2013-03-03"), as.Date("2014-09-17"), by="1 day")
  mondays <- format(dates[weekdays(dates)=="Monday"], "%y%m%d")

  # pblapply gives us progress bars for free!

  do.call("rbind", pblapply(mondays, function(day) {

    freak <- html_session(sprintf("http://torrentfreak.com/top-10-most-pirated-movies-of-the-week-%s/", day))
    if (freak$response$status_code >= 400) {
      freak <- html_session(sprintf("http://torrentfreak.com/top-10-pirated-movies-week-%s/", day))
    }

    data.frame(date=as.Date(day, format="%y%m%d")-1,
               movie=freak %>% html_nodes("td:nth-child(3)") %>% html_text() %>% .[1:10],
               rank=freak %>% html_nodes("td:nth-child(1)") %>% html_text() %>% .[2:11],
               rating=freak %>% html_nodes("td:nth-child(4)") %>% html_text() %>% .[1:10],
               imdb.url=freak %>% html_nodes("td:nth-child(4) a[href*='imdb']") %>% html_attr("href") %>% .[1:10],
               stringsAsFactors=FALSE)

  }))

}

# in the spirit of reproducible research and to avoid having to "remember" what one did
# in a text editor to clean up a file, a cleanup function like this one is extrememly
# valuable. the data can be regenerated at any time (provided it's still scrapeable) and
# and when some new condition arises (in this case some new "rip types" appeared over the
# coure of time)

cleanUpMovieData <- function(imdb) {

  # all of this work on the title is prbly not necessary since we just end up using the Title from the OMDB
  # but, I did this first so I'm keeping it in. dagnamit

  imdb$movie <- gsub("^\ +|\ +$", "", iconv(imdb$movie, to="UTF-8"))

  # stupid factors get in the way sometimes
  imdb[] <- lapply(imdb, as.character)

  # elimiante the "rip types"
  imdb$movie <- gsub("\ * \\((Camaudio|Cam audio|CAM|Cam|CAM/R5|CAM/TS|Cam/TS|DVDscr|DVDscr/BrRip|DVDscr/DVDrip|HDCAM|HDTS|R6|R6/CAM|R6/Cam|R6/TS|TS|TS/Cam|TS/Webrip|Webrip|Webrip/TS|HDrip/TS)\\)", "", imdb$movie, ignore.case=TRUE)

  # normalize case & punctuation, though some of this isn't really necessary since
  # we have the IMDB id and can get the actual "real" title that way, but this is
  # an OK step if we didn't have that other API to work with (and didn't during the
  # initial building of the example)

  imdb$movie <- gsub("’", "'", imdb$movie)
  imdb$movie <- gsub(" a ", " a ", imdb$movie, ignore.case=TRUE)
  imdb$movie <- gsub(" of ", " of ", imdb$movie, ignore.case=TRUE)
  imdb$movie <- gsub(" an ", " an ", imdb$movie, ignore.case=TRUE)
  imdb$movie <- gsub(" and ", " and ", imdb$movie, ignore.case=TRUE)
  imdb$movie <- gsub(" is ", " is ", imdb$movie, ignore.case=TRUE)
  imdb$movie <- gsub(" the ", " the ", imdb$movie, ignore.case=TRUE)
  imdb$movie <- gsub("Kick Ass", "Kick-Ass", imdb$movie, fixed=TRUE)
  imdb$movie <- gsub("Part III", "Part 3", imdb$movie, fixed=TRUE)
  imdb$movie <- gsub("\\:", "", imdb$movie)
  imdb$movie <- gsub("\ +", " ", imdb$movie)

  # the IMDB rating is sometimes wonky
  imdb$rating <- gsub(" /.*$", "", imdb$rating)
  imdb$rating <- gsub("?.?", NA, imdb$rating, fixed=TRUE)
  imdb$rating <- as.numeric(imdb$rating)

  # need some things numeric and as dates
  imdb$rank <- as.numeric(imdb$rank)

  imdb$date <- as.Date(imdb$date)

  imdb$imdb.url <- str_extract(imdb$imdb.url, "(tt[0-9]+)")

  # use decent column names efficiently thanks to data.table
  setnames(imdb, colnames(imdb), c("date", "movie", "rank", "rating", "imdb.id"))

  imdb

}

# call out to the OMDB API for rotten tomatoes and other bits of info

getOMDBInfo <- function(imdb.ids) {

  do.call("rbind", pblapply(unique(imdb.ids), function(imdb.id) {

    dat <- GET(sprintf("http://www.omdbapi.com/?i=%s&tomatoes=TRUE", imdb.id))
    data.frame(fromJSON(content(dat, as="text")), stringsAsFactors=FALSE)

  }))

}

# makes 10K 10000 (etc)
# adapted from http://stackoverflow.com/a/15015037/1457051

currencyToNumeric <- function(vector) {
  vector <- as.character(vector) %>% gsub("(\\$|,| )", "", .) %>% as.numeric
  k_positions <- grep("K", vector, ignore.case=TRUE)
  result[k_positions] <- as.numeric(gsub("K", "", vector[k_positions])) * 1000
  m_positions <- grep("M", vector, ignore.case=TRUE)
  result[m_positions] <- as.numeric(gsub("M", "", vector[m_positions])) * 1000000
  return(result)
}

# yes, even the OMDB data needs a touch up and conversion to R objects

cleanUpOMDB <- function(omdb) {

  omdb$imdbVotes <- as.numeric(gsub(",", "", omdb$imdbVotes))
  omdb$tomatoUserReviews <- as.numeric(gsub(",", "", omdb$tomatoUserReviews))

  for(col in c("Metascore", "imdbRating", "tomatoUserRating",
               "tomatoMeter", "tomatoRating", "tomatoReviews",
               "tomatoFresh", "tomatoRotten", "tomatoUserMeter")) {
    omdb[,col] <- as.numeric(omdb[,col])
  }

  omdb$BoxOffice <- currencyToNumeric(omdb$BoxOffice)

  omdb$DVD <- as.Date(omdb$DVD, format="%d %b %Y")
  omdb$Released <- as.Date(omdb$Released, format="%d %b %Y")

  omdb$Rated <- factor(omdb$Rated)
  omdb$Runtime <- as.numeric(gsub("\ *min", "", omdb$Runtime))

  omdb

}

# grab all the posters for these movies. needs latest httr
# since that one makes it easy to save data to files
# we don't get progress bars for free this time

downloadPosters <- function(combined, .progress=TRUE) {

  posters <- combined %>% select(imdb.id, Poster) %>% unique

  invisible(mapply(function(id, img) {
    dest_file <- sprintf("data/posters/%s.jpg", id)
    if (!file.exists(dest_file)) {
      if (.progress) {
        message(img)
        GET(img, write_disk(dest_file), progress("down"))
      } else {
        GET(img, write_disk(dest_file))
      }
    }
  }, posters$imdb.id, posters$Poster))

}


# makes violin plots of some interesting variables

movieRanges <- function(movies, title="") {

  comb <- movies %>%
    select(short.title, rank, rating, Rated, Runtime, Metascore, imdbRating, imdbVotes,
           tomatoMeter, tomatoRating, tomatoReviews, tomatoFresh, tomatoRotten, BoxOffice) %>%
    group_by(short.title) %>% filter(row_number()==1) %>% ungroup

  comb$Rated <- as.numeric(comb$Rated)

  comb <- data.frame(short.title=as.character(comb$short.title), scale(comb[-1]))

  comb_melted <- comb %>% melt(id.vars=c("short.title"))

  cols <- colnames(comb)[-1]

  for(x in cols) {
    x <- as.character(x)
    y <- range(as.numeric(movies[, x]), na.rm=TRUE)
    comb_melted$variable <- gsub(x, sprintf("%s\n[%s:%s]", x,
                                            prettyNum(floor(y[1]), big.mark=",", scientific=FALSE),
                                            prettyNum(floor(y[2]), big.mark=",", scientific=FALSE)),
                                            as.character(comb_melted$variable))
  }

  gg <- comb_melted %>% ggplot(aes(x=variable, y=value, group=variable, fill=variable))
  gg <- gg + geom_violin()
  gg <- gg + coord_flip()
  gg <- gg + labs(x="", y="")
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(axis.text.x=element_blank())
  gg <- gg + theme(axis.text.y=element_text(size=16))
  gg <- gg + theme(axis.ticks.x=element_blank())
  gg <- gg + theme(axis.ticks.y=element_blank())
  if (title != "") { gg <- gg + labs(title=title) }
  gg

}

# no need to scrape/clean/combine if we've already scraped/cleaned/combined
# could be enhanced to check for latest date in the rda and add to it if it's
# a later date now. left as an exercise for the reader.

if (!file.exists("data/combined.rda")) {

  imdb.scraped <- scrapeMovieData()
  imdb <- cleanUpMovieData(imdb.scraped)

  omdb.info <- getOMDBInfo(imdb$imdb.id)
  omdb <- cleanUpOMDB(omdb.info)

  # merge pirate data and omdb data

  combined <- merge(imdb, omdb, by.x="imdb.id", by.y="imdbID")
  combined$Title <- gsub("é", "e", combined$Title) # handle Le Mis
  combined <- combined %>% group_by(Title) %>% mutate(freq=n()) %>% .$freq

  combined$short.title <- abbreviate(combined$Title, minlength=14)
  combined$short.title <- factor(combined$short.title,
                                 levels=unique(combined$short.title[order(-combined$freq)],
                                               ordered=TRUE))

  combined$freq <- factor(combined$freq)

  # some dates were missing

  combined[combined$Title=="12 Years a Slave",]$DVD <- as.Date("2014-03-04")
  combined[combined$Title=="Breakout",]$DVD <- as.Date("2013-09-17")
  combined[combined$Title=="Dead in Tombstone",]$DVD <- as.Date("2013-10-22")
  combined[combined$Title=="Dhoom: 3",]$DVD <- as.Date("2014-04-15")
  combined[combined$Title=="Ender's Game",]$DVD <- as.Date("2014-02-11")
  combined[combined$Title=="Epic",]$DVD <- as.Date("2013-08-20")
  combined[combined$Title=="Iron Man: Rise of Technovore",]$DVD <- as.Date("2013-04-16")
  combined[combined$Title=="Once Upon a Time in Mumbai Dobaara!",]$DVD <- as.Date("2013-10-26")
  combined[combined$Title=="Redemption",]$DVD <- as.Date("2013-09-24")
  combined[combined$Title=="Rise of the Guardians",]$DVD <- as.Date("2013-03-12")
  combined[combined$Title=="Scavengers",]$DVD <- as.Date("2013-09-03")
  combined[combined$Title=="Shootout at Wadala",]$DVD <- as.Date("2013-06-15")
  combined[combined$Title=="Sleeping Beauty",]$DVD <- as.Date("2012-04-10")
  combined[combined$Title=="Son of Batman",]$DVD <- as.Date("2014-05-06")
  combined[combined$Title=="Stand Off",]$DVD <- as.Date("2013-03-26")
  combined[combined$Title=="Tarzan",]$DVD <- as.Date("2014-08-05")
  combined[combined$Title=="The Hangover Part III",]$DVD <- as.Date("2013-10-08")
  combined[combined$Title=="The Wicked",]$DVD <- as.Date("2013-04-30")
  combined[combined$Title=="Welcome to the Punch",]$DVD <- as.Date("2013-05-08")

  # some ratings were missing and/or incorrect

  combined[combined$Title=="Bad Country",]$Rated <- "R"
  combined[combined$Title=="Breakout",]$Rated <- "R"
  combined[combined$Title=="Dhoom: 3",]$Rated <- "Unrated"
  combined[combined$Title=="Drive Hard",]$Rated <- "PG-13"
  combined[combined$Title=="Once Upon a Time in Mumbai Dobaara!",]$Rated <- "Unrated"
  combined[combined$Title=="Scavengers",]$Rated <- "PG-13"
  combined[combined$Title=="Shootout at Wadala",]$Rated <- "Unrated"
  combined[combined$Title=="Sleeping Beauty",]$Rated <- "Unrated"
  combined[combined$Title=="Sparks",]$Rated <- "Unrated"
  combined[combined$Title=="Street Fighter: Assassin's Fist",]$Rated <- "Unrated"
  combined[combined$Title=="The Colony",]$Rated <- "R"
  combined[combined$Title=="The Last Days on Mars",]$Rated <- "R"
  combined[combined$Title=="The Physician",]$Rated <- "PG-13"

  # normalize the ratings (Unrated == Not Rated)

  combined[combined$Rated=="Not Rated", "Rated"] <- "Unrated"
  combined$Rated <- factor(as.character(combined$Rated))

  save(combined, file="data/combined.rda")

} else {

  load("data/combined.rda")

}

downloadPosters(combined)

# 1. Let's see a how a line plot over time looks ###################

# ordering short.title factor by how many weeks it was on the piRate chaRts

combined$short.title <- reorder(combined$short.title, -as.numeric(combined$freq))

combined %>%
  select(Title, rank, date) %>%
  ggplot(aes(x=date, y=rank)) +
  scale_y_reverse(breaks=c(10:1)) +
  scale_x_date(expand=c(0,0)) +
  geom_line(aes(color=Title)) +
  labs(x="", y="Rank", title="PiRate Movie Ranks over Time") +
  theme_bw() + theme(legend.position="none", panel.grid=element_blank())

# 2. ugh, perhaps if we highlight some movies ######################

drt <- combined %>%  select(Title, rank, date) %>% mutate(color="Not Selected")

selected_titles <- c("Frozen",
                     "Captain America: The Winter Soldier",
                     "The Amazing Spider-Man 2",
                     "Star Trek Into Darkness",
                     "The Hobbit: An Unexpected Journey",
                     "The Hobbit: The Desolation of Smaug")

drt[drt$Title %in% selected_titles,]$color <- drt[drt$Title %in% selected_titles,]$Title
drt$color <- factor(drt$color, levels = c("Not Selected", selected_titles), ordered = TRUE)

ggplot(drt, aes(x=date, y=rank, group=Title)) +
  geom_line(aes(color=color)) +
  scale_x_date(expand=c(0,0)) +
  scale_y_reverse(breaks=c(10:1)) +
  scale_color_manual(values=c("#e7e7e7", brewer.pal(length(selected_titles), "Dark2")),
                     name="Movie") +
  theme_bw() + theme(legend.position="bottom", legend.direction="vertical",
                     panel.grid=element_blank())

# 3. now see the see the # weeks distribution #####################

combined %>% select(Title, freq) %>%
  unique %>% ggplot(aes(x=freq)) +
  geom_histogram(aes(fill=freq)) +
  labs(x="# Weeks on ChaRts", y="Movie count") +
  theme_bw() +
  theme(legend.position="none")

# 4. faceted plot of movies with more than 4 weeks on the pirate top 10 charts ########

gg <- ggplot(data=combined %>% filter(as.numeric(freq)>=4), aes(x=date, y=rank, group=short.title))
gg <- gg + geom_segment(aes(x=date, xend=date, y=10, yend=rank, color=freq), size=0.25)
gg <- gg + geom_point(aes(color=freq), size=1)
gg <- gg + scale_color_brewer(palette = "Paired", name="# Weeks on PiRate ChaRts")
gg <- gg + scale_fill_brewer(palette = "Paired", name="# Weeks on PiRate ChaRts")
gg <- gg + scale_y_reverse(label=floor)
gg <- gg + labs(x="", y="", title="PiRated Weekly Movie Rankings : March 2013 - September 2014")
gg <- gg + facet_wrap(~short.title, ncol=10)
gg <- gg + theme_bw()
gg <- gg + theme(text=element_text(family="Gotham Medium"))
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks.x=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(legend.position="top")

movies <- ggplotGrob(gg)
note <- textGrob("Grey region defines period\nbetween movie theater release\nand DVD/streaming release", gp=gpar(fontsize=14, fontfamily="Gotham Medium"))

plot.layout <- gtable_filter(movies, 'panel', trim=FALSE)$layout
movies <- gtable_add_grob(movies, note, t=max(plot.layout$t),
                          l=tail(plot.layout, 1)$r+1, r=max(plot.layout$l))

grid.newpage()
grid.draw(movies)

# 5. let's look at genres #######################################

genres <- combined %>% select(Title, Genre) %>% unique %>% .$Genre

genre_table <- as.data.frame(table(genres), stringsAsFactors = FALSE)
colnames(genre_table) <- c("Genre", "Count")

genre_table %>% arrange(desc(Count)) %>% head(10)

# 6. now graph them ##########################################

gg1 <- ggplot(genre_table, aes(xend=reorder(Genre, Count), yend=Count))
gg1 <- gg1 + geom_segment(aes(x=reorder(Genre, Count), y=0))
gg1 <- gg1 + geom_point(aes(x=reorder(Genre, Count), y=Count))
gg1 <- gg1 + scale_y_continuous(expand=c(0,0.5))
gg1 <- gg1 + labs(x="", y="", title="Movie counts by full genre classification")
gg1 <- gg1 + coord_flip()
gg1 <- gg1 + theme_bw()
gg1 <- gg1 + theme(panel.grid=element_blank())
gg1 <- gg1 + theme(panel.border=element_blank())
gg1

# 7. a view of single sub-genres ##############################

single_genres <-  as.data.frame(table(unlist(strsplit(genre_table$Genre, ",\ *"))),
                                stringsAsFactors=FALSE)
colnames(single_genres) <- c("Genre", "Count")
gg1 <- ggplot(single_genres, aes(xend=reorder(Genre, Count), yend=Count))
gg1 <- gg1 + geom_segment(aes(x=reorder(Genre, Count), y=0))
gg1 <- gg1 + geom_point(aes(x=reorder(Genre, Count), y=Count))
gg1 <- gg1 + scale_y_continuous(expand=c(0,0.5))
gg1 <- gg1 + labs(x="", y="")
gg1 <- gg1 + coord_flip()
gg1 <- gg1 + theme_bw()
gg1 <- gg1 + theme(panel.grid=element_blank())
gg1 <- gg1 + theme(panel.border=element_blank())
gg1

# 8. How many week's past DVD release date? ###################

combined <- combined %>% group_by(Title) %>% mutate(weeks.past=sum(date>DVD)) %>% ungroup

# 9. Look at single sub-genres again, but by weeks past ###################

gg2 <- as.data.frame(table(unlist(strsplit(combined %>%
                                      filter(!is.na(weeks.past) & weeks.past>1) %>%
                                      select(Title, Genre) %>% unique %>% .$Genre, ",\ *"))),
              stringsAsFactors = FALSE) %>%
  arrange(desc(Freq)) %>% ggplot(aes(xend=reorder(Var1, Freq), yend=Freq))
gg2 <- gg2 + geom_segment(aes(x=reorder(Var1, Freq), y=0))
gg2 <- gg2 + geom_point(aes(x=reorder(Var1, Freq), y=Freq))
gg2 <- gg2 + scale_y_continuous(expand=c(0,0.5))
gg2 <- gg2 + labs(x="", y="")
gg2 <- gg2 + coord_flip()
gg2 <- gg2 + theme_bw()
gg2 <- gg2 + theme(panel.grid=element_blank())
gg2 <- gg2 + theme(panel.border=element_blank())
gg2

# 10. the treasure lies in dates, so get in and beyond DVD date info #############

combined.beyond <- combined %>% group_by(Title) %>% filter(date > DVD) %>% ungroup

grid.arrange(movieRanges(combined, "All Top 10 PiRate Movies"),
             movieRanges(combined.beyond, "Still in Top 10 Charts After\nPiRated AfteR DVD Release"), ncol=2)

# 11. quick look at movies past DVD with some metadata #############

dat <- combined %>%
  filter(!is.na(weeks.past), weeks.past>1) %>% group_by(Title) %>%
  filter(date==max(date)) %>% ungroup %>%
  select(Title, Genre, weeks.past, Rated, rank, Metascore, BoxOffice,
         tomatoMeter, imdbVotes, imdbRating) %>%
  group_by(Title) %>%
  filter(rank==min(rank), !is.na(Metascore), !is.na(BoxOffice),
         !is.na(imdbVotes), !is.na(tomatoMeter)) %>%
  unique %>% arrange(weeks.past)

dat

# 12. final plot - this shows the most significant reason for being on the top 10

gg <- ggplot(data=combined %>% filter(as.numeric(freq)>=4, !is.na(DVD)), aes(x=date, y=rank, group=short.title))
gg <- gg + geom_rect(aes(xmin=Released, xmax=DVD, ymin=0, ymax=10), fill="#dddddd", alpha=0.25)
gg <- gg + geom_segment(aes(x=Released, xend=Released, y=0, yend=10), color="#7f7f7f", size=0.125)
gg <- gg + geom_segment(aes(x=DVD, xend=DVD, y=0, yend=10), color="#7f7f7f", size=0.125)
gg <- gg + geom_segment(aes(x=date, xend=date, y=10, yend=rank, color=freq), size=0.25)
gg <- gg + geom_point(aes(color=freq), size=1)
gg <- gg + scale_color_brewer(palette = "Paired", name="# Weeks on PiRate ChaRts")
gg <- gg + scale_fill_brewer(palette = "Paired", name="# Weeks on PiRate ChaRts")
gg <- gg + scale_y_reverse(label=floor)
gg <- gg + labs(x="", y="", title="PiRated Weekly Movie Rankings : March 2013 - September 2014")
gg <- gg + facet_wrap(~short.title, ncol=10)
gg <- gg + theme_bw()
gg <- gg + theme(text=element_text(family="Gotham Medium"))
gg <- gg + theme(strip.background=element_blank())
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks.x=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(legend.position="top")
gg


# 13. poster histograms

# get all the #1 hits & sort them by box office receipts
number_one <- combined %>% group_by(Title) %>% filter(rank==1, rating==max(rating)) %>% select(Title, short.title, imdb.id, rank, rating, BoxOffice) %>% ungroup %>% unique
number_one <- number_one[complete.cases(number_one),] %>% arrange(desc(BoxOffice))

# read in all their poster images
posters <- sapply(number_one$imdb.id, function(x) readJpeg(sprintf("data/posters/%s.jpg", x)))

# calculate the max bin count so we can normalize the histograms across RGB plots & movies
hist_max <- max(sapply(number_one$imdb.id, function(x) {
  max(hist(posters[[x]][,,1], plot=FALSE, breaks=seq(from=0, to=260, by=10))$counts,
      hist(posters[[x]][,,2], plot=FALSE, breaks=seq(from=0, to=260, by=10))$counts,
      hist(posters[[x]][,,3], plot=FALSE, breaks=seq(from=0, to=260, by=10))$counts)
}))

# plot the histograms with the poster, labeling with short title and $
n<-nrow(dat)
png("data/posters/histograms.png", width=3600, height=1800)
plot.new()
par(mar=rep(2, 4))
par(mfrow=c(n/3, 12))
for (i in 1:12) {
  for (j in 1:3) {
    plot(posters[[i*j]])
    hist(posters[[i*j]][,,1], col="red", xlab = "", ylab = "", main="", breaks=seq(from=0, to=260, by=10), ylim=c(0,hist_max))
    hist(posters[[i*j]][,,2], col="green", xlab = "", ylab = "", main=sprintf("%s - %s", dat[i*j,]$short.title, dollar(dat[i*j,]$BoxOffice)), breaks=seq(from=0, to=260, by=10), ylim=c(0,hist_max))
    hist(posters[[i*j]][,,3], col="blue", xlab = "", ylab = "", main="", breaks=seq(from=0, to=260, by=10), ylim=c(0,hist_max))
  }
}
dev.off()
