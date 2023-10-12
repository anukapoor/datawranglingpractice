library(dplyr)
#data loading
rail <-
  read.csv("~/Downloads/Rail_Equipment_Accident_Incident_Data.csv")

x <- colnames(rail)
sort(x, decreasing = FALSE)
missing_values <- sapply(rail, function(x)
  sum(is.na(x)))
print(missing_values)
sort(missing_values, decreasing = TRUE)

#SUBSETTING THE DATA
rail_cleaned_data <-
  rail[c(
    "State.Code",
    "State.Name",
    "County.Name",
    "Accident.Type",
    "Visibility",
    "Total.Persons.Injured",
    "Total.Persons.Killed",
    "Passengers.Injured",
    "Passengers.Killed",
    "Train.Speed",
    "Accident.Cause",
    "Contributing.Accident.Cause"
  )]
#View(rail_cleaned_data)


words <- unlist(strsplit(rail_cleaned_data$Accident.Cause, " "))

word_freq <- table(words)

max_freq <- max(word_freq)

words_with_max_freq <- names(word_freq)
sort(words_with_max_freq, decreasing = TRUE)

words <- unlist(strsplit(rail_cleaned_data$Accident.Cause, " "))


word_freq <- table(words)

word_freq_df <-
  data.frame(Word = names(word_freq), Frequency = as.numeric(word_freq))

word_freq_df <- word_freq_df[order(-word_freq_df$Frequency),]

your_word_df <- "[Dd]efective"
rail_cleaned_data$word_count_df <-
  stringr::str_count(tolower(rail_cleaned_data$Accident.Cause), your_word_df)
sum_df <- sum(rail_cleaned_data$word_count_df)

your_word_br <- "[Bb]roken"
rail_cleaned_data$word_count_br <-
  stringr::str_count(tolower(rail_cleaned_data$Accident.Cause), your_word_br)
sum_br <- sum(rail_cleaned_data$word_count_br)

your_word_ms <- "[Mm]issing"
rail_cleaned_data$word_count_ms <-
  stringr::str_count(tolower(rail_cleaned_data$Accident.Cause), your_word_ms)
sum_ms <- sum(rail_cleaned_data$word_count_ms)

your_word_fl <- "[Ff]ailure"
rail_cleaned_data$word_count_fl <-
  stringr::str_count(tolower(rail_cleaned_data$Accident.Cause), your_word_fl)
sum_fl <- sum(rail_cleaned_data$word_count_fl)

word_count_names <-
  c(your_word_df, your_word_br , your_word_ms , your_word_fl)
word_count_freq <- c(sum_df, sum_br, sum_ms, sum_fl)
data.frame(word_count_ranking, stringsAsFactors = TRUE)
word_count_ranking <- data.frame(word_count_names, word_count_freq)


word_count_ranking <-
  word_count_ranking[order(-word_count_ranking$word_count_freq),]

library(ggplot2)
ggplot(word_count_ranking,
       aes(
         x = reorder(word_count_names, -word_count_freq),
         y = word_count_freq,
         fill = word_count_names
       )) +
  geom_bar(stat = "identity") +
  xlab("Words") +
  ylab("Frequency") +
  ggtitle("Word Frequency Ranking") +
  theme_minimal()


rail_cleaned_data$total_affected <-
  rail_cleaned_data$Total.Persons.Injured +
  rail_cleaned_data$Total.Persons.Killed

top_five <- rail_cleaned_data[order(-rail_cleaned_data$total_affected), ]$State.Name[1:5]

top_five <- rail_cleaned_data %>% 
group_by(State.Name) %>%
  summarize(total_affected = sum(total_affected)) %>%
  arrange(desc(total_affected)) %>%
  head(5)


ggplot(top_five, aes(x = reorder(State.Name,-total_affected), y = total_affected, fill=total_affected)) +
  geom_bar(stat = "identity") +
  labs(title = "Accident Rate by State", x = "State.Name", y = "total_affected") +
  theme_minimal()