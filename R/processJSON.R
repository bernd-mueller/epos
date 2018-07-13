library(jsonlite) 

url = "inst/similaritydata/mesh-epilepsy/meshep.json"

out <- lapply(readLines(url), fromJSON)


jdmesh <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("concept", "doccount")
jdmesh <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(jdmesh) <- x
jddrug <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(jddrug) <- x

for(i in 1:length(out)) {
  cur <- out[i]
  ucur <- unlist(cur)
  id <- ucur[1]
  doccount <- ucur[2]
  concept <- ucur [3]
  if (endsWith(id, "DrugBank")){
    jddrug[nrow(jddrug) + 1,] = c(concept, doccount)
  }
}
