
# attach packages
packages <- c("ggplot2","ggsci","reshape2","socialmixr")
lapply(packages, require, character.only = TRUE)

# list available surveys on Zenodo
list_surveys()

# load a survey using its Zenodo DOI
vietnam_survey <- get_survey("https://doi.org/10.5281/zenodo.1289473")
data(polymod)

# save a downloaded survey as RDS
saveRDS(vietnam_survey, "vietnam.rds")

# list countries contained in the survey
survey_countries(vietnam_survey)
survey_countries(polymod)

# get reference of survey
cite(vietnam_survey)

# obtain a contact matrix
contact_matrix(polymod, countries = "United Kingdom", 
               age.limits = c(0, 1, 5, 15))

# obtain a symetric contact matrix
contact_matrix(polymod, countries = "United Kingdom", 
               age.limits = c(0, 1, 5, 15), symmetric = TRUE)

# split a contact matrix
contact_matrix(polymod, countries = "United Kingdom", 
               age.limits = c(0, 1, 5, 15), split = TRUE)

# bootstrapping `n` contact matrices with specified age brackets
m <- contact_matrix(polymod, countries = "United Kingdom", 
                    age.limits = c(0, 5, 13, 18, 30, 40, 50, 65), n=100)

# derive quintiles (e.g. mean) from bootstrapped matrices
mr <- Reduce("+", lapply(m$matrices, function(x) {x$matrix})) / length(m$matrices)
mr

# plotting 
df <- melt(mr, varnames = c("age1", "age2"), value.name = "contacts")
ggplot(df, aes(x = age2, y = age1, fill = contacts)) + 
  theme(legend.position = "bottom") + geom_tile()

v <- contact_matrix(vietnam_survey, age.limits = c(0, 5, 18, 65), n = 100)
vr <- Reduce("+", lapply(v$matrices, function(x) {x$matrix})) / length(v$matrices)
df.v <- melt(vr, varnames = c("age1","age2"), value.name = "contacts")
ggplot(df.v, aes(x = age2, y = age1, fill = contacts)) +
  theme(legend.position = "bottom") + geom_tile()
