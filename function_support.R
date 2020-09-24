split.data <- function(data, p = 0.7, s = 1){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train=train, test=test))
}

process_data <- function(dataset) {
  ### Get boolean from categorical variable
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$gender), function(x){(dataset$gender == x)*1})))
  # names(flags) = levels(dataset$gender)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$NationalITy), function(x){(dataset$NationalITy == x)*1})))
  # names(flags) = levels(dataset$NationalITy)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$PlaceofBirth), function(x){(dataset$PlaceofBirth == x)*1})))
  # names(flags) = levels(dataset$PlaceofBirth)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$StageID), function(x){(dataset$StageID == x)*1})))
  # names(flags) = levels(dataset$StageID)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$GradeID), function(x){(dataset$GradeID == x)*1})))
  # names(flags) = levels(dataset$GradeID)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$SectionID), function(x){(dataset$SectionID == x)*1})))
  # names(flags) = levels(dataset$SectionID)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$Topic), function(x){(dataset$Topic == x)*1})))
  # names(flags) = levels(dataset$Topic)
  # dataset = cbind(dataset, flags)####
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$Semester), function(x){(dataset$Semester == x)*1})))
  # names(flags) = levels(dataset$Semester)
  # dataset = cbind(dataset, flags)####
  # 
  flags = data.frame(Reduce(cbind,
                            lapply(levels(dataset$Relation), function(x){(dataset$Relation == x)*1})))
  names(flags) = levels(dataset$Relation)
  dataset = cbind(dataset, flags)
  
  flags = data.frame(Reduce(cbind,
                            lapply(levels(dataset$ParentAnsweringSurvey), function(x){(dataset$ParentAnsweringSurvey == x)*1})))
  names(flags) = levels(dataset$ParentAnsweringSurvey)
  dataset = cbind(dataset, flags)
  # 
  # flags = data.frame(Reduce(cbind, 
  #                           lapply(levels(dataset$ParentschoolSatisfaction), function(x){(dataset$ParentschoolSatisfaction == x)*1})))
  # names(flags) = levels(dataset$ParentschoolSatisfaction)
  # dataset = cbind(dataset, flags)####
  
  flags = data.frame(Reduce(cbind, 
                            lapply(levels(dataset$StudentAbsenceDays), function(x){(dataset$StudentAbsenceDays == x)*1})))
  names(flags) = levels(dataset$StudentAbsenceDays)
  dataset = cbind(dataset, flags)
  
  dataset$highAndament = dataset$Class == "H"
  dataset$mediumAndament = dataset$Class == "M"
  dataset$lowAndament = dataset$Class == "L"
  
  #dataset.processed = data.frame(dataset[, 10:13], dataset[, 17:88])
  dataset.processed = data.frame(dataset[, 10:11], dataset$Discussion, dataset[, 17:26])
  return(dataset.processed)
}
