# thegameplan

## results <- analysis(data)
For this problem set, your task is write out pseudocode for the analyses you'll need for your project. Think about all of the steps you'll need to transform your raw data into the central summary measures and statistical tests that will reveal your amazing findings. What summary measures do you need to answer your research question? Do you need to recode variables (e.g., recode responses into correct vs. incorrect), manage potentially missing data, or try to find outliers? Write out the logical steps in an organized and structured format. If you don't know all the details of your analysis, that is fine. Go with something that's sensible. But, the closer you are to the actual analysis you will implement, the more helpful this assignment will be later on!

## Deliverables
Your submission should be 1 text file with:
- A brief paragraph explaining the raw data from your experiment (what are your IVs and DVs?) and the summary measures you'll need to compute (e.g., accuracy, median reaction times, average confidence ratings)
- Your pseudocode that details how you'll turn raw data into findings

[Inspiration](https://www.youtube.com/watch?v=VcjzHMhBtf0)

# Paragraph with a description

The participants in my study will complete a verbal 1-, 2- and 3-back with letter stimuli. Reaction time and accuracy are collected for each response.
There are 2 possible trial types in the 1-back (targets and non-targets) and 3 possible tiral types in the 2- and 3-back (lures, targets, and non-targets). 
Accuracy is coded as 0 and 1, and reaction time is in milliseconds. I want to analyze trial by trial performance (using the lmer and glmer functions), so 
accuracy needs to be recoded to -1 and 1 (-1 for incorrect responses, and 1 for correct responses). Additionally, I want to use basic anovas to see if 
overall average RT and accuracy is significantly different for each n-back type. For this analysis, I will need to create a summary data frame with the 
average RT and accuracy for each subject (rather than using a dataset with trial-by-trial responses). Furthermore, participants in my study will be filling out 
a demographic questionnaire. I need to link their demographic data to their behavioural data, and combine all the individual participant files into one "master" 
datafile.  


# Compile/concatenate the individual participant files into one large dataframe  
# all files are csv and end in _data
behavioural_data <- compile(*_data.csv)
 
# need to merge a separate dataset with the demographic data with the behavioural data 
# merge the two datasets by subject 
dataset <- concatenate(behavioural_data & demographic_data, by = subject)

# interested in analyzing the RT on correct trials, so create a dataset with only the correct responses – analyze RT
# trim the reaction times so that RTs less than or equal to 100ms
dataset_correct <- remove(dataset, where dataset$correct == 0 & dataset&RT ? 100)
 
# For each subject, trim the reaction times that are 2.5 SDs above and below the mean
dataset_correct <-  for (RT in each subject$dataset_correct) {
if(RT > 2.5 * SD(mean(RT)) then remove(RT)
}

# create a summary dataframe with the mean RT and accuracy for each subject  
summary_data <- summarize(dataset_correct by subject)

# Visualize the data to see if there are any major outliers – super long RTs or overall low mean accuracy: use a boxplot with a labelling function to identify which participant the outlying value belongs to
boxplot(summary_data $RT, label(subject))
boxplot(summary_data $mean_accuracy, label(subject)) 

# Log transform the RTs so they are normally distributed in the trial-by-trial performance dataset (i.e. dataset_correct)
dataset_correct$RT <- log_transform(dataset_correct$RT)

# how does RT and accuracy on the 1-, 2-, and 3-back compare? 
nback.anova.accuracy <- Anova(accuracy ~ n_type, data = summary_data)
summary(nback.anova.accuracy)

nback.anova.RT <- Anova(RT ~ n_type, data = summary_data)
summary(nback.anova.RT)

# how do the trial types within each n-back condition compare? 
nback.anova.accuracy.interaction <- Anova(accuracy ~ n_type*trial_type, data = summary_data)
summary(nback.anova.accuracy.interaction)

nback.anova.RT.interaction <- Anova(RT ~ n_type*trial_type, data = summary_data)
summary(nback.anova.RT.interaction)

# visualize the results with bargraphs 
bar_graph(RT, x = n_type, data = summary_data) #graph for overall performance in each condition

bar_graph(RT, x = n_type, fill_of_bars = trial_type, data = summary_data) #graph for performance broken down by trial type

bar_graph(accuracy, x = n_type, data = summary_data) #graph for overall performance in each condition

bar_graph(accuracy, x = n_type, fill_of_bars = trial_type, data = summary_data) #graph for performance broken down by trial type

# for the trial by trial analysis, need to recode the accuracy data – currently incorrect responses are 0, and correct responses are 1 
# need to recode so that incorrect responses are -1 and correct responses are 1 
dataset_correct$accuracy <- recode(0 to -1)

# are there group differences in lure accuracy and lure RT (the three groups are BSO, BSOE2 and AMC)?
# using mixed linear models controlling for age, and history of cancer treatments (chemotherapy, tamoxifen, radiation)
# need to subset so we can analyze 2-back and 3-back lure accuracy and RTs
2lure.accuracy <- glmer(accuracy ~ group + age + cancer_treatments + (1|subject), family = binomial, data = dataset_correct, subset = (n_type == “2-back” & trial_type == “lure”))
summary(2lure.accuracy)

3lure.accuracy <- glmer(accuracy ~ group + age + cancer_treatments + (1|subject), family = binomial, data = dataset_correct, subset = (n_type == “3-back” & trial_type == “lure”))
summary(3lure.accuracy)

2lure. RT <- lmer(RT ~ group + age + cancer_treatments + (1|subject), data = dataset_correct, subset = (n_type == “2-back” & trial_type == “lure”))
summary(2lure. RT)

3lure. RT <- lmer(RT ~ group + age + cancer_treatments + (1|subject), data = dataset_correct, subset = (n_type == “3-back” & trial_type == “lure”))
summary(3lure. RT)

# visualize the results with bargraphs 
bar_graph(RT, x = group, data = summary_data, subset = (n_type == “2-back” & trial_type == “lure”)) 

bar_graph(RT, x = group, data = summary_data, subset = (n_type == “3-back” & trial_type == “lure”)) 

bar_graph(accuracy, x = group, data = summary_data, subset = (n_type == “2-back” & trial_type == “lure”)) 

bar_graph(accuracy, x = group, data = summary_data, subset = (n_type == “3-back” & trial_type == “lure”)) 

# is there a relationship between estradiol levels and performance on lure trials?
correlation(estradiol, accuracy, control_for = age & cancer_treatments, data = dataset_correct, subset = (n_type == “2-back” & trial_type == “lure”)) # for the 2-back condition 

correlation(estradiol, accuracy, control_for = age & cancer_treatments, data = dataset_correct, subset = (n_type == “3-back” & trial_type == “lure”)) # for the 3-back condition 

# create plots for the correlations
scatter_plot(estradiol, accuracy, data = dataset_correct, subset = (n_type == “2-back” & trial_type == “lure”))

scatter_plot(estradiol, accuracy, data = dataset_correct, subset = (n_type == “3-back” & trial_type == “lure”))
