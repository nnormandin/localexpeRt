# test of localexpeRt functions using QSAR modeling data from Applied Predictive Modeling
# credit to Max Kuhn, author of Applied Predictive Modeling

require(AppliedPredictiveModeling)

# bring data into environment
data(solubility)

# rename for simplicity
x <- solTrainXtrans
y <- solTrainY

# examine data
dim(x) # 951 instances with 228 features
glimpse(x) # use glimpse(x) to look at feature types

# locate NA values
sum(sapply(x, function(x) sum(is.na(x)))) # no NA values, ok to proceed

# plot the response variable
hist(y, main = '', xlab = 'Solubility') #  ok to proceed

# convert the response into binary columns
# use equally probable width, output 20 columns
y.cols <- BinCols(y, n = 20, mode = 'EP')

# examine breakpoints over histogram of response
hist(y, main = '', xlab = 'Solubility')
abline(v = y.cols$y.vals)

# train Local Experts on each of the binary columns
# use default train control object with Support Vector Machines algorithm and linear kernel
model.list <- TrainLEs(x, y.cols$cols, method = "svmLinear")

# examine ensemble performance by extracting all model info
model.info <- ExtractModelInfo(model.list)

# plot the model information
PlotLEs(model.info)

