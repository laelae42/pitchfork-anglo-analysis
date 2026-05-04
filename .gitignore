library(dplyr)
library(readr)
library(ggplot2)

pitchfork <- read_csv('~/Downloads/Pitchfork_sample_data.csv')

pitchfork_clean <- pitchfork %>%
  # Primary Genre: convert to factor
  mutate(
    primary_genre = factor(primary_genre,
                           levels = c('Pop/R&B',
                                      'Electronic',
                                      'Rock',
                                      'Rap',
                                      'Experimental',
                                      'Metal',
                                      'Folk/Country',
                                      'Jazz',
                                      'Global')
    )) %>%
  # generate binary variables (Anglo = 1, non-Anglo = 0) and convert to factor
  mutate(is_anglo = if_else(country %in% c('US', 'GB'), 1, 0)) %>%
  mutate(is_anglo = factor(is_anglo,
                           levels = c(0,1),
                           labels = c('Non-Anglo', 'Anglo')))


# H1: Non-Anglo artists will constitute below 25% of artists represented in Pitchfork’s album reviews.

pitchfork_clean %>%
  group_by(is_anglo) %>%
  summarise(
    n = n(),
    proportion = round(n/nrow(pitchfork_clean)*100, 1)
  ) 

## visualisation

pitchfork_clean %>%
  group_by(is_anglo) %>%
  summarise(n=n()) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x = is_anglo, y= proportion, fill = is_anglo)) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 0.25, linetype = 'dashed', colour = col_threshold) +
  scale_fill_manual(values = c('Non-Anglo' = col_non_anglo, 'Anglo' = col_anglo)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Proportion of Anglo vs. Non-Anglo Artists',
    x = NULL,
    y = 'Proportion', 
    caption = 'Dashed line = 25% threshold (H1)'
  ) +
  theme_pitchfork() +
  theme(legend.position = 'none')


# H2: Among non-Anglo artists reviewed by Pitchfork, more than 30% will be classified under the “Global” genre tag.

genre_anglo_table <- pitchfork_clean %>%
  group_by(primary_genre, is_anglo) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(primary_genre) %>%
  mutate(proportion = n / sum(n))

genre_anglo_table

## Chi-square test (primary_genre ~ is_anglo)

chi_table <- table(pitchfork_clean$primary_genre, pitchfork_clean$is_anglo)

chisq.test(chi_table)

## Genre distribution of non-Anglo artists

pitchfork_clean %>%
  filter(is_anglo == 'Non-Anglo') %>%
  group_by(primary_genre) %>%
  summarise(n = n()) %>%
  mutate(proportion = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(proportion))

## Visualisation (Anglo/non-Anglo proportion per genre)

genre_anglo_table %>%
  ggplot(aes(x = primary_genre,
             y = proportion,
             fill = is_anglo)) +
  geom_col(position = 'stack') +
  scale_fill_manual(values = c(
    'Non-Anglo' = col_non_anglo,
    'Anglo' = col_anglo
  )) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Anglo vs. Non-Anglo Artist Representation by Genre',
    x = NULL,
    y = 'Proportion',
    caption = 'Based on Pitchfork album reviews (n=270)'
  ) +
  theme_pitchfork() +
  theme(axis.text.x = element_text(angle=35, hjust=1))

# H3: There will be no statistically significant difference in Pitchfork review scores between Anglo and non-Anglo artists. 

pitchfork_clean %>%
  group_by(is_anglo) %>%
  summarise(
    n    = n(),
    mean = round(mean(review_score, na.rm = T), 2),
    sd   = round(sd(review_score, na.rm = T), 2),
    min  = min(review_score, na.rm = T),
    max  = max(review_score, na.rm = T)
  )

## t-test
t.test(review_score ~ is_anglo,
       data        = pitchfork_clean,
       alternative = 'two.sided',
       var.equal   = F) 

# Multiple regression (genre dummy coding)
pitchfork_clean <- pitchfork_clean %>%
  mutate(primary_genre = relevel(primary_genre, ref = 'Rock'))

model <- lm(review_score ~ is_anglo + primary_genre,
            data = pitchfork_clean)

summary(model)

