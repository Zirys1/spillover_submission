library(xkcd)
library(extrafont)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(scales)
library(stringr)
library(egg)
library(ggrepel)
library(tidytext)
common_ops <-  list(theme(plot.title = element_text(size=8),
                          axis.text.y = element_text(size = 8, hjust = 1),
                          axis.text.x = element_text(size = 8, hjust = 0.5),
                          axis.title = element_text(size = 8),
                          strip.text = element_text(size = 8),
                          legend.position="bottom",
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text()))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
Sys.setlocale("LC_TIME", "C")
all_tweets <- map(2009:2018, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  tbl_df()

all_tweet_words <- all_tweets %>%
  mutate(text = str_replace_all(text, "https?://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  filter(!str_detect(text, "^(\"|RT)")) %>%
  unnest_tokens(word, text, token = "regex", pattern = "reg") %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

campaign_tweets <- all_tweets %>%
    filter(created_at >= "2015-06-16") %>%
    mutate(source = str_replace(source, "Twitter for ", "")) %>%
    filter(!str_detect(text, "^(\"|RT)"))

ct <- campaign_tweets %>%
  group_by(month = round_date(created_at, "month")) %>%
  filter(month >= "2016-12-01") %>% 
  summarize(tweets = n(),
            fake_news = sum(str_detect(str_to_lower(text), "fake news")),
            ratio = sum(str_detect(str_to_lower(text), "fake news")/tweets)*100) %>% 
  ggplot(aes(x =month, y = ratio)) +
  geom_line(size = 1) +
  geom_point(shape = 15, color = "darkgrey", size = 2) +
  scale_x_datetime(labels = date_format("%b %Y"), date_breaks = "3 months") +
  scale_y_continuous(breaks = c(seq(0,10,2)), limits = c(0,11)) +
  theme_bw()+
  ylab("Percentage of tweets\nusing the term 'fake news'") +
  xlab(NULL) +
  ggtitle("A: Usage of term 'fake news' in tweets")  +
  common_ops +
  theme(panel.grid.major.x = element_line(size=.1, colour = "grey"),
        panel.grid.major.y = element_blank())

# Get sentiment of “fake news” tweets
# filter out tweets that include 'fake news'
data("stop_words")
st <- campaign_tweets %>%
  group_by(month = round_date(created_at, "month")) %>% 
  filter(str_detect(str_to_lower(text), "fake news")) %>% #select only tweets where he uses words 'fake news'
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  summarise(meansent = mean(score)) 

fst <- campaign_tweets %>%
  group_by(month = round_date(created_at, "month")) %>% 
  filter(month >= "2016-12-01") %>% #select only tweets where he uses words 'fake news'
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  summarise(meansent = mean(score)) 

# fst and st in one graph
st$FakeNews <- rep(1,nrow(st))
fst$FakeNews <- rep(0,nrow(fst))
std <- rbind(st,fst)
std$FakeNews <- factor(std$FakeNews, levels = c(0,1), labels = c("Excluding FN", "Only FN"))

ftd <- ggplot(std, aes(x =month, y = meansent, color = FakeNews)) +
  geom_line(size = 1) +
  #  scale_y_continuous(breaks = c(seq(-0.6, 0.6, 0.1))) +
  geom_point(shape = 15, color = "darkgrey", size = 2) +
   theme_bw(base_size =5)+
   ggtitle("B: Sentiment score, including and excluding 'fake news'-tweets")  +
   ylab("AFINN\nsentiment score") +
   xlab(NULL) +
   scale_color_grey() +
   scale_x_datetime(labels = date_format("%b %Y"), date_breaks = "3 months") +
   guides(fill = guide_legend(title.position="top")) +
   common_ops +
   theme(panel.grid.major.x = element_line(size=.1, colour = "grey"),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(), 
        legend.position = c(0.83,0.16),
        legend.text =  element_text(size=6),
        legend.background = element_rect(fill = "transparent", color = NA, size=.5))

# multiplot(ct, ftd, cols = 1)
ggarrange(ct, ftd, ncol = 1)

tikz(file = paste0(getwd(), "/Fig1alt.tex"), width = 4.5, height = 4.7)
ggarrange(ct, ftd, ncol = 1)
dev.off()