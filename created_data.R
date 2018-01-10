library(ADNIMERGE)

data = subset(adnimerge,select = c('RID','DX','Month','ICV','WholeBrain','Hippocampus','Ventricles','Entorhinal','MMSE','ADAS11','FAQ'))
data$DX = as.numeric(data$DX)

volumetric = c('WholeBrain','Hippocampus','Entorhinal','Ventricles')

for (i in volumetric){ data[[i]] = data[[i]]/data$ICV}


for (i in c('WholeBrain','Hippocampus','Entorhinal','MMSE')){ data[[i]] = -data[[i]]}

RID_to_exclude = list()
RID_to_include = list()

for (test_rid in unique(data$RID)){ 
    xxx = sapply(subset(data,RID==test_rid), function(s) (any(sum(is.na(s)))/length(subset(data,RID==5295)$Month!=1)) & sum(!is.na(s))>1 )
    if (any(xxx[5:length(xxx)] == FALSE))
	{ RID_to_exclude = append(RID_to_exclude,test_rid) }
    else
        { RID_to_include = append(RID_to_include,test_rid)}
}

variables_to_include = c('RID','Month','WholeBrain','Hippocampus','Ventricles','Entorhinal','MMSE','ADAS11','FAQ','DX')
out_data = subset(data, RID%in%RID_to_include,select = variables_to_include)

out_data = out_data[-9263,]

write.csv(out_data,"ADNI_test.csv")
