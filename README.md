# StatisticalAnalyses
Samples for different R-studio analysis functions

See the Table of Contents Excel Document





##RegressionModels&predicting_examples:
	##functions used: filter(), select(), mutate(), glm(), lm(), binomial(), round(), $coefficients, AIC(), lm.beta(), table(), summary(), $standardized.coefficients, cor.test(), na.omit(), cor(), print(), 
	##concepts involved:
		## predicting missing values using existing data, narrowing predictors
		## lm.beta example; identifying strongest predictors
		## AIC examples
		## Data Cleaning example and quick correlation test; predicting missing values using existing data
		## logistic regression modeling
		## logistic regression predicting

##Summarise&GroupBy_examples:
	##functions used: head(), select(), arrange(), summarise(), group_by(), filter(), mutate(), mutate_if(), max(), mean(), as.data.frame()
	##concepts involved:
		## using summarise and group_by to inspect data
		## manipulating data using summarise and group by




##OutlierIdentification_example:
	##functions used: quantile(), which(), nrow(), length(), cor.test(), lm(), summary(), lm.beta()
	##concepts involved:
		## identifying outliers
		## removing outliers
		## setting aside outliers
		## creating a model after adjusting for outliers


##GraphAesthetics:
	##functions used: +theme(), +labs(), +scale_fill_manual(), +scale_x_continuous(), +scale_y_continuous(), geom_chicklet()
	##concepts involved:
		## removing legend
		## removing axis labels
		## changing legend label
		## changing axis labels
		## change colors
		## add a bold line for the axis
		## changing tick mark labels
		## making rounded bar graphs


## Colors&ColorPaletteCreation:
	##functions used: rainbow(), heat.colors(), terrain.colors(), topo.colors(), cm.colors(), barplot(), colorRampPalette(), ggplot(), +scale_color_gradient2(), +scale_color_gradient(), +scale_fill_gradient(), qplot(), aes(), mean(), geom_point(), scale_fill_hue(), display.brewer.all(), wes.palette(), +scale_fill_brewer(), +scale_color_brewer(), scale_color_grey(), scale_fill_grey(), theme_classic()
	##concepts involved:
		## creating palettes between two colors
		## using gradient colors on a scatterplot
		## changing order of colors
		## diverging color scheme
		## adjusting lightness, color intensity, and hue
		## manually assigning colors
		## using color brewer
		## using grayscale
		## using wesanderson


## CompleteDataProject_wCleaining+Outliers+Regression_example
	##functions used: cor(), mutate_if(), AIC(), na.omit(), glm(), binomial(), filter(), select(), mutate(), transmute(), $coefficients, update(), $null.deviance, $deviance, $df.null, $df.residual, pchisq(), cat(), summary(), sd(), mean(), max(), sum(), min(), group_by(), cor.test(), as.matrix(), rcorr(), ggplot(), geom_point(), +facet_wrap(), geom_col(), aes(), stat_count(), geom_boxplot(), corrplot(), length(), which(), quantile(), rename(), aov(), TukeyHSD(), lm(), as.data.frame(), nrow(), round(), summarise(), summary(), ols_step_all_possible() 
	##concepts involved:
		## data cleaning
		## outlier identification
		## outlier removal
		## evaluating effects of outlier removal
		## using AIC and R-squared for predictor selection
		## using mutate, transmute, summarise, group_by, and filter() to create new data using the existing data (metadata)
		## boxplot creation
		## exploring data with boxplot
		## ANOVA tests
		## regression model creation
		## logistic regression model creation
		## predicting values using statistical models
		## different methods for outlier identification and removal
		## visualizing distributions with stat_count()
		## creating correlation matrix
		## creating comparative graphs using facet_wrap(); visually comparing predictors
		## basic descriptive statistics
		## Chi-Squared test
