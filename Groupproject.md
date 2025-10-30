Does where you live affect your chances of having asthma?
================
Elise Johnson
2025-10-30

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [First analysis](#first-analysis)
  - [Second analysis/plot](#second-analysisplot)
- [DISCUSSION](#discussion)
  - [Interpretation 1 -](#interpretation-1--)
  - [Interpretation 2 -](#interpretation-2--)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

# BACKGROUND

# STUDY QUESTION and HYPOTHESIS

## Questions

Is there a correlation between the proportion of low air quality days
and prevalence of asthma per state?

## Hypothesis

Where you live has an effect on whether you experience asthma or not

## Prediction

I predict that the state you live in does impact your chances of having
asthma. The states with lower air quality will have higher rates of
asthma.

# METHODS

## First analysis

## Second analysis/plot

# DISCUSSION

## Interpretation 1 -

## Interpretation 2 -

# CONCLUSION

# REFERENCES

my_data \<- read.csv(“groupfile.csv”)

head(my_data)

names(my_data)

ggplot(my_data, aes(x = State, y =
Percent.Adults.with.Current.Asthma)) + geom_point() + theme_minimal()
