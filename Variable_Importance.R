#set.seed(242)
#split = rsample::initial_split(data, prop = 0.7, strata = Class)
#train_data = rsample::training(split)
#test_data = rsample::testing(split)
# Remove labels from test data, save them in variables unlabeled_test_data and test_labels
#test_labels = test_data$Class # Save test dataset labels
#unlabeled_test_data = subset(x = test_data, select = -c(Class)) # Remove target variable from test data
################################################################################
############################# SHANNON ENTROPY ##################################
################################################################################

# train/test model 
#shannon_tree = ImbTreeEntropyKaniadakis(Y_name = "Class", 
#                                        X_names = colnames(train_data)[-ncol(train_data)], 
#                                        data = train_data, 
#                                        depth = 50,
#                                        min_obs = 1, 
#                                        type = "Shannon", entropy_par = 1,
#                                        cp = 0, n_cores = 1, weights = NULL, cost = NULL, 
#                                        class_th = "equal", overfit = "prune", cf = 0.25)

#shannon_predict_tree = PredictTree(shannon_tree, unlabeled_test_data) # prediction
#cm_shannon = confusionMatrix(shannon_predict_tree$Class, test_labels)
#PrintTree(shannon_tree)
#######################################################################################################
# $measure corresponds to (each) entropy value f.ex here is Shannon Entropy
shannon_tree$`hrs_sitting <= 0.06`$measure 
ExtractRules(shannon_tree)$Rule
shannon_tree$children$`hrs_sitting >  0.06`$measure
shannon_tree$root$measure







