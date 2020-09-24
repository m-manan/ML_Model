library("PCAmixdata")
library("factoextra")
library("FactoMineR")
library("ggplot2")
library("earth")

dataset = read.csv("xAPI-Edu-Data.csv", header = TRUE, stringsAsFactors = FALSE)


# Change some attribute's value for avoid duplicates
dataset$Semester[dataset$Semester == "F"] = "First"
dataset$Semester[dataset$Semester == "S"] = "Second"
dataset$gender[dataset$gender == "M"] = "Male"
dataset$gender[dataset$gender == "F"] = "Female"
dataset$NationalITy[dataset$NationalITy == "Egypt"] = "EG"
dataset$NationalITy[dataset$NationalITy == "Iran"] = "IN"
dataset$NationalITy[dataset$NationalITy == "Iraq"] = "IQ"
dataset$NationalITy[dataset$NationalITy == "Jordan"] = "JO"
dataset$NationalITy[dataset$NationalITy == "lebanon"] = "LE"
dataset$NationalITy[dataset$NationalITy == "Lybia"] = "LY"
dataset$NationalITy[dataset$NationalITy == "Morocco"] = "MA"
dataset$NationalITy[dataset$NationalITy == "Palestine"] = "PA"
dataset$NationalITy[dataset$NationalITy == "SaudiArabia"] = "SA"
dataset$NationalITy[dataset$NationalITy == "Tunis"] = "TU"
dataset$NationalITy[dataset$NationalITy == "USA"] = "AM"
dataset$NationalITy[dataset$NationalITy == "venzuela"] = "VE"
dataset$NationalITy[dataset$NationalITy == "Syria"] = "SY"

# Factorize categorical attributes
dataset$gender = factor(dataset$gender)
dataset$NationalITy = factor(dataset$NationalITy)
dataset$PlaceofBirth = factor(dataset$PlaceofBirth)
dataset$StageID = factor(dataset$StageID)
dataset$GradeID = factor(dataset$GradeID)
dataset$SectionID = factor(dataset$SectionID)
dataset$Topic = factor(dataset$Topic)
dataset$Semester = factor(dataset$Semester)
dataset$Relation = factor(dataset$Relation)
dataset$ParentAnsweringSurvey = factor(dataset$ParentAnsweringSurvey)
dataset$ParentschoolSatisfaction = factor(dataset$ParentschoolSatisfaction)
dataset$StudentAbsenceDays = factor(dataset$StudentAbsenceDays)
dataset$Class = factor(dataset$Class)


# Divide numerical and categorical attributes
dataset.quali = splitmix(dataset)$X.quali
dataset.quanti = splitmix(dataset)$X.quanti

### PCA
res.pca = prcomp(dataset.quanti, center = TRUE,scale. = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

### PCA MIX
res.pcamix <- PCAmix(X.quanti = dataset.quanti, X.quali = dataset.quali, rename.level = TRUE)

plot(res.pcamix, choice = "ind", coloring.ind = dataset$Class, label = FALSE, main = "Observations")
plot(res.pcamix, choice = "levels", xlim = c(-1.5,2.5), main = "Levels")
plot(res.pcamix, choice = "cor", main = "Numerical variables")
plot(res.pcamix, choice = "sqload", coloring.var = T, leg = FALSE, main = "All variables")
### END PCA MIX


### PCA FACTOMINER
res.famd <- FAMD(dataset, graph = FALSE, ncp = 10)
eig.val <- get_eigenvalue(res.famd)
var <- get_famd_var(res.famd)
quanti.var <- get_famd_var(res.famd, "quanti.var")
quali.var <- get_famd_var(res.famd, "quali.var")
ind <- get_famd_ind(res.famd)

fviz_screeplot(res.famd)
# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)
# Contribution to the third dimension
fviz_contrib(res.famd, "var", axes = 3)

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_mfa_ind(res.famd, 
             habillage = "Class", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
)
### END PCA FACTOMINER


### FEATURE SELECTION WITH MARS MODEL
marsModel <- earth(as.factor(Class) ~ ., data=dataset, degree = 1)
summary(marsModel)
plot(marsModel, wich = 1, nresponse = 1)

ev <- evimp (marsModel)
plot(ev)

### Correlation ###
library("vcd")
st <- structable(Class ~ StudentAbsenceDays + ParentAnsweringSurvey + Relation, dataset)
pairs(st)
