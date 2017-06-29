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
  #sim.mat <- 1- sim.mat
  matlist = c( matlist, list(sim.mat))
}
diss.matlist <- list()
for (mat in matlist) {
  mat = mat / max(max(mat))
  diss.matlist <- c(diss.matlist,list(mat))
}
saveRDS(matlist,"data/DissimilarityMatrixList.RData")
init_mat <- array(0,dim = c(9,dim(matlist[[1]])[1], dim(matlist[[1]])[1]))
for (k in 1:9){
  diag(init_mat[k, , ]) = 1
  print(head(init_mat[k, ,]))
}
init_mat <- array(0,dim = c(1,dim(matlist[[1]])[1], dim(matlist[[1]])[1]))

diag(init_mat[1, , ]) = 1
#  print(head(init_mat[1, ,]))
library(doSNOW)


cl <- parallel::makeCluster(8) #change the 2 to your number of CPU cores
nmc = 8

registerDoSNOW(cl)
for (i in 1:3)
  for (j in 2:4){
    omni.mat <- OmnibusEmbed::omnibusM.Kcond(diss.matlist[c(i,j)],init_mat)
    w.val.vec <- 0.8
    rep.i <- 1
    size.vec <- seq(0, 1, 0.05)
    
    first_result <- OmnibusEmbed::run.JOFC.match.jacknife.replicate (m.i = rep.i, N = 239, test.samp.size = 20, 
                                                     w.val.len = 1, 
                                                     Diss.E = matlist[[i]], Diss.F = matlist[[j]], 
                                                     d = 3,   oos = TRUE,   separability.entries.w = TRUE
                                                     , wt.equalize = FALSE, assume.matched.for.oos = TRUE
                                                     , oos.use.imputed = FALSE, 
                                                     w.vals = w.val.vec, size = size.vec, 
                                                     verbose = TRUE, level.critical.val = 0.05)
  }

    jk.res.IsoJ <- foreach (rep.i = 1:nmc) %dopar% {
    

      
      
    res[[rep.i]]  <- OmnibusEmbed::run.JOFC.match.jacknife.replicate (m.i = rep.i, N = 239, test.samp.size = 20, 
                                         w.val.len = 1, 
                                         Diss.E = matlist[[i]], Diss.F = matlist[[j]], 
                                         d = 3,   oos = TRUE,   separability.entries.w = FALSE
                                         , wt.equalize = FALSE, assume.matched.for.oos = TRUE
                                         , oos.use.imputed = FALSE, 
                                         w.vals = w.val.vec, size = size.vec, 
                                         verbose = TRUE, level.critical.val = 0.05)

  }
  
    
}