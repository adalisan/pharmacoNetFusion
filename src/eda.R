library('ProjectTemplate')
load.project()

source("./src/real.data.experiment.fn.R")
download.file("http://cancerres.aacrjournals.org/highwire/filestream/335671/field_highwire_adjunct_files/2/177323_1_supp_3899876_bmkvbb.xlsx", destfile = "data/simmatrix.xlsx")

matlist = list()
entities = list()
for ( sh in 1:4 ) {
  sim.mat <- readxl::read_excel("data/simmatrix.xlsx", sheet = sh )
  rownames(sim.mat) <- as.vector(t(sim.mat[,1]))
  sim.mat <- sim.mat[,-1]
  entities <- union(colnames(sim.mat), entities)
  sim.mat <- as.matrix(sim.mat)
  
  matlist = c( matlist, list(sim.mat))
}
init_mat <- array(0,dim = c(9,dim(matlist[[1]])[1], dim(matlist[[1]])[1]))
for (k in 1:9){
  diag(init_mat[k, , ]) = 1
  print(head(init_mat[k, ,]))
}
omni.mat<-OmnibusEmbed::omnibusM.Kcond(matlist[1:4],init_mat)