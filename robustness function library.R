# Robustness metric function library
# Phase III report
# created Jan-March 2021
# created by Nathan Bonham

library(nsga2R) # for non-dominated sort function


####################### Satisficing ###############################
###################################################################

satisficing=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                     thresholds=c(600, 10, 5), fail_if_inequality=rep('greater', length(objectives)), 
                     n_satisficing=length(objectives), policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=2 # one column for satisficing plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', 'satisficing')
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  if(n_satisficing > length(objectives)){
    
    n_satisficing=length(objectives)
    print('n_satisficing must be <= number of objectives. n_satisficing has been changed to length(objectives).')
    
  }
  
  if(length(fail_if_inequality) != length(objectives)){
    
    fail_if_inequality=rep('greater', length(objectives))
    print('length(fail_if_inequality) must = length(objectives). fail_if_inequality has been changed to rep(greater, length(objectives)).')
    
  }
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute satisficing #################################
    
      satisficing_calcs=matrix(ncol=length(objectives)+2, nrow=nrow(filter.policy))
      
      for (cr in 1:length(objectives)){
        
        if (fail_if_inequality[cr]=='greater'){
          
          satisficing_calcs[,cr]=ifelse(filter.policy[,which(colnames(filter.policy)==objectives[cr])] > thresholds[cr], 0,1) # a 1 means 'met criteria'
          
        } else if (fail_if_inequality[cr]=='less'){
          
          satisficing_calcs[,cr]=ifelse(filter.policy[,which(colnames(filter.policy)==objectives[cr])] < thresholds[cr], 0,1) # a 1 means 'met criteria'
          
        } else {
          
          satisficing_calcs[,cr]=NA
          
        }
        

      } # satisficing criteria loop 
      
      if (length(objectives)==1){ # only one objective, don't need to do rowSums operator
        satisficing_calcs[, length(objectives)+1]=satisficing_calcs[,1]
        satisficing_calcs[, length(objectives)+2]=ifelse(satisficing_calcs[, length(objectives)+1] < n_satisficing , 0, 1) # a 1 in final column means yes, satisficing
      } else { # more than one objective. Use rowSums
        satisficing_calcs[, length(objectives)+1]=rowSums(satisficing_calcs[,1:length(objectives)])
        satisficing_calcs[, length(objectives)+2]=ifelse(satisficing_calcs[, length(objectives)+1] < n_satisficing , 0, 1) # a 1 in final column means yes, satisficing
      }

      metric.df$satisficing[r]=sum(satisficing_calcs[,ncol(satisficing_calcs)])/nrow(satisficing_calcs)
      
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


############################ Satisficing deviation ##################################
#####################################################################################


satisficing.deviation=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                     thresholds=c(600, 10, 5), fail_if_inequality=c('greater', 'greater', 'greater'), 
                     policy_ID_column=2, SOW_column=1, SOW_agg_method='mean',
                     percentile=0.9){
  
  ####### prepare data frame to store results
  
  ncol=2 # one column for satisficing plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', 'satisficing.deviation')
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute satisficing deviation #################################
    
    baseline_performance=thresholds

    sat_dev_calcs=matrix(ncol=length(objectives)+1, nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      
      if (fail_if_inequality[cr]=='greater'){
        
        sat_dev_calcs[,cr]=(filter.policy[,which(colnames(filter.policy)==objectives[cr])] - baseline_performance[cr])/baseline_performance[cr] # if greater than fails criteria, actual - baseline 
        
      } else if (fail_if_inequality[cr]=='less'){
        
        sat_dev_calcs[,cr]=(baseline_performance[cr]- filter.policy[,which(colnames(filter.policy)==objectives[cr])] )/baseline_performance[cr] # if less than fails criteria, baseline - actual
        
      } else {
        
        sat_dev_calcs[,cr]=NA
        
      }
      
      
    } # satisficing deviation criteria loop  
    
    
    if (length(objectives)==1){ # only one objective, don't need to do rowSums operator
      sat_dev_calcs[, length(objectives)+1]=sat_dev_calcs[,1]
    } else { # more than one objective. Use rowSums
      sat_dev_calcs[, length(objectives)+1]=rowSums(sat_dev_calcs[,1:length(objectives)]) 
    }
    ######### SOW aggregation ################
    
    if (SOW_agg_method=='percentile'){
      
      metric.df$satisficing.deviation[r]=quantile(sat_dev_calcs[,ncol(sat_dev_calcs)],percentile)
      
    } else {
      agg_func=match.fun(SOW_agg_method) # transform string into function. 
      metric.df$satisficing.deviation[r]=agg_func(sat_dev_calcs[,ncol(sat_dev_calcs)])
      
    }
    
    ######## end SOW aggregation #############
    

  } # end policy loop
  
  return(metric.df)
  
} # end function


###################### Regret from best performance ##############################
##################################################################################


regret2=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                               best_if=c('min', 'min', 'min'), 
                               policy_ID_column=2, SOW_column=1, SOW_agg_method='mean',
                 percentile=0.9, Obj_agg_method='mean'){
  
  
  ############ Calculate best and worst performance in every SOW, for each objective
  
  SOW_iter=unique(data[,SOW_column])
  regret2_data=data[,c(policy_ID_column,SOW_column,which(colnames(data) %in% objectives))] # remove objectives we are not using
  
  best=matrix(nrow=length(SOW_iter), ncol=ncol(regret2_data)-2) # preallocated matrix to store best performance values of objective i in SOW j
  worst=matrix(nrow=length(SOW_iter), ncol=ncol(regret2_data)-2) # preallocated matrix to store worst performance values
  
  # find optimal performance for objective i in SOW j. Also find worst performance for objective i in SOW j to calculate max deviation later  
  ######### You should remove this part from the policy loop. Redundant computation #################
  for (i in 1:length(SOW_iter)){
    filter.SOW=regret2_data[which(regret2_data[,2]==SOW_iter[i]),]
    
    for (c in 1:ncol(best)){ # loop through objectives, find min or max as best performance
      
      if (best_if[c]=='min'){
        best[i,c]=min(filter.SOW[,(2+c)])
        worst[i,c]=max(filter.SOW[,(2+c)])
      } else if (best_if[c]=='max'){
        best[i,c]=max(filter.SOW[,(2+c)])
        worst[i,c]=min(filter.SOW[,(2+c)])
      } else {
        paste('ERROR: this function only supports min or max for finding best objective performance in regret type II calculations')
      }
      
    } # end objectives loop
    
    
  } # end SOW iteration
  
  
  ############## end of computing best and worst matrices ###############
  
  ####### prepare data frame to store results
  
  if (Obj_agg_method=='none'){
    ncol=length(objectives)+1 # one per objective plus one for policy ID
  } else { # use default aggregation (normalize and sum)
    ncol=2 # one column for satisficing plus one for policy ID
  }
  
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', 'regret2')
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute type 2 regret #################################

    regret2_calcs=matrix(ncol=length(objectives)+1, nrow=nrow(filter.policy))
    
    
    for (cr in 1:length(objectives)){ # loop through criteria
      
      numerator=abs(filter.policy[,which(colnames(filter.policy)==objectives[cr])] - best[,cr])
      # denominator=filter.policy[,which(colnames(filter.policy)==objectives[cr])] # I don't like this as the normalizing factor
      
      ##### max deviation across all policies for each SOW #####################
      
      max_deviation=best[,cr] - worst[,cr] # this is the maximum deviation across all POLICIES for a given SOW (the rows). Ie, best performance in SOW j across all policies minus worst performance in SOW j across all policies
      
      # max_deviation=range(data[,which(colnames(data)==objectives[cr])])[2]-range(data[,which(colnames(data)==objectives[cr])])[1] # this isn't SOW specific
      denominator=abs(max_deviation) # scale everything zero to 1 by dividing by the largest deviation possible across all Policy and SOW
      
      regret2_calcs[,cr]=numerator/denominator
      
      denom_0=which(round (denominator, 2)==0) # anywhere the demoninator is zero, the result is NaN.
      
      regret2_calcs[denom_0,cr]=numerator[denom_0] # if denominator is zero, just use numerator
      
      
    } # end regret2 criteria loop  
    
    if (length(objectives)==1){
      regret2_calcs[, length(objectives)+1]=regret2_calcs[,1]
    } else {
      regret2_calcs[, length(objectives)+1]=rowSums(regret2_calcs[,1:length(objectives)])
    }
    
    
    ######### SOW aggregation ################
    
    if (SOW_agg_method=='percentile'){
      
      metric.df$regret2[r]=quantile(regret2_calcs[,ncol(regret2_calcs)],percentile)
      
    } else {
      agg_func=match.fun(SOW_agg_method) # transform string into function. 
      metric.df$regret2[r]=agg_func(regret2_calcs[,ncol(regret2_calcs)])
      
    }
    
    ######## end SOW aggregation #############
    
  } # end policy loop
  
  best=data.frame(best)
  worst=data.frame(worst)
  colnames(best)=objectives
  colnames(worst)=objectives
  
  return(list(regret2=metric.df, best_per_SOW=best, worst_per_SOW=worst))
  
} # end function


####################### Percent deviation from baseline #############################
#####################################################################################

percent_deviation=function(data=obj, baseline=MOEA_data, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                 percentile=90, max_objectives='none',
                 policy_ID_column=2, SOW_column=1, zero_tolerance=0.001){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### loop through objectives #################################
    
    # calcs=matrix(ncol=length(objectives), nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      
      if (objectives[cr]==max_objectives){ # maximization objective
        
        fmin=quantile(filter.policy[[objectives[cr]]], probs=(1-percentile/100))
        base=baseline[[objectives[cr]]][r]
        
        deviation=base-fmin
        
        metric.df[r,(cr+1)]=deviation/base
        
      } else { # minimization objective
        
        fmax=quantile(filter.policy[[objectives[cr]]], probs=percentile/100)
        base=baseline[[objectives[cr]]][r]
        
        deviation=fmax-base
        
        metric.df[r,(cr+1)]=deviation/base
        
      } # end max or min if statement
      
      ####### if statement to handle base = 0
      
      if (abs(base) < zero_tolerance){ # if base performance is essentially zero
        if (abs(deviation) < zero_tolerance){ # if deviation is zero
          
          metric.df[r,(cr+1)] = 0 # set deviation to zero. Need to override deviation/base = 0/0 = NaN
          
        } else { # deviation is > 0, but base is 0. Set to 999
          
          metric.df[r,(cr+1)] = 999
          
        }
        
      }
      
      
    } #  criteria loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function






####################### Maximin ###############################
###################################################################

# default percentiles assume minimization. If maximization, change percentile to 0

maximin=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                     percentile=rep(100, length(objectives)), 
                     policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute maximin #################################
    
    
    calcs=matrix(ncol=length(objectives), nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      

      metric.df[r,(cr+1)]=quantile( filter.policy[,which(colnames(filter.policy)==objectives[cr])], percentile[cr]/100) #return the desired quantile. For case of minimization and maximin, want 100%

    } #  criteria loop  
    

    
  } # end policy loop
  
  return(metric.df)
  
} # end function


####################### Maximax ###############################
###################################################################

# same as maximin, but default percentile is 0. Note that if maximization objectives, would switch percentile to 100

maximax=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                 percentile=c(0,0,0), 
                 policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #  plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute maximax #################################
    
    
    calcs=matrix(ncol=length(objectives), nrow=nrow(filter.policy))
    
    for (cr in 1:length(objectives)){
      
      
      metric.df[r,(cr+1)]=quantile( filter.policy[,which(colnames(filter.policy)==objectives[cr])], percentile[cr]/100) #return the desired quantile. For case of minimization and maximin, want 100%
      
    } # criteria loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


############################# Hurwicz optimism-pessimism rule ##################
###############################################################################

HurwiczOP=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                 best_case_weight=0.5, best_if=rep('min', length(objectives)), 
                 policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  
  worst_case_weight=1-best_case_weight
  
  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute metric #################################
    
    
    for (cr in 1:length(objectives)){
      
      min=min(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      max=max(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      
      if(best_if[cr]=='min'){
        
        metric.df[r,(cr+1)]=(best_case_weight*min + worst_case_weight*max)/2
        
      } else if (best_if[cr]== 'max'){
        
        metric.df[r,(cr+1)]=(best_case_weight*max + worst_case_weight*min)/2
        
      } else {
        print('This function only supports max or min for the best_if values')
      }
      
      
      
    } #  objective loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


############################# Laplace's principle of insufficient reason #########################
##################################################################################################


LaplacePIR=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                   policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through
  

  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute metric #################################
    
    
    for (cr in 1:length(objectives)){

      metric.df[r, (cr+1)]=mean(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      
    } #  objective loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function

############################# Mean - variance ############################
##########################################################################
## See Hamarat et al 2014
# In case of minimization, use mean*sd. In maximization, use mean/sd 


mean_variance=function(data=obj, objectives=c('LB.Shortage.Volume', 'Mead.1000', 'Powell.3490'),
                    best_if=rep('min', length(objectives)), policy_ID_column=2, SOW_column=1){
  
  ####### prepare data frame to store results
  
  ncol=length(objectives)+1 #plus one for policy ID
  nrow=length(unique(data[, policy_ID_column])) # one row per policy
  
  metric.df=matrix(NA, nrow=nrow, ncol=ncol)
  metric.df=data.frame(metric.df)
  colnames(metric.df)=c('policy', objectives)
  metric.df[,1]=unique(data[, policy_ID_column])
  
  policy_iter=unique(data[, policy_ID_column]) # id of policies to loop through

  
  for (r in 1:length(policy_iter)){ ########## loop through policies
    
    filter.policy=data[which(data[,policy_ID_column]==policy_iter[r]),]
    
    ##################### Compute metric #################################
    
    
    for (cr in 1:length(objectives)){
      
      mean=mean(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      sd=sd(filter.policy[,which(colnames(filter.policy)==objectives[cr])])
      
      if(best_if[cr]=='min'){
        
        metric.df[r,(cr+1)]=mean*sd
        
      } else if (best_if[cr]== 'max'){
        
        metric.df[r,(cr+1)]=mean/sd
        
      } else {
        print('This function only supports max or min for the best_if values')
      }
      
      
    } #  objective loop  
    
    
    
  } # end policy loop
  
  return(metric.df)
  
} # end function


################### function to add non-dominated tier level to data.frame of choice ########################
#############################################################################################################

add_NonDom_front=function(data, policy_ID_column=1, max_cols=NULL){
  
  front=rep(NA, nrow(data))
  
  if(is.null(max_cols)==FALSE){ # fastNonDOminatedSorting assumes minimization. Multiply be negative 1 for metrics that should be maximized
    
    data[,max_cols]=data[,max_cols]*-1
    
  }
  
  front_list=fastNonDominatedSorting(data[-policy_ID_column])
  
  nfronts=length(front_list)
  
  for (i in 1:nfronts){
    
    policies=front_list[[i]]
    front[policies]=i
    
  }
  
  data$front=front
  
  
  if(is.null(max_cols)==FALSE){ # convert back to positive where necessary
    
    data[,max_cols]=data[,max_cols]*-1
    
  }
  
  return(data)
  
}



