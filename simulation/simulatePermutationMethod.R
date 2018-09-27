library(ape)
library(castor)
library(phangorn)
library(caper)

cutCladesR = function(tree,node,tip_states,res=c()){
  desc = unlist(Descendants(tree,node,type="tips"))
  if(length(unique(tip_states[desc]))==1){
    return(paste(desc,collapse="-"))
  } else{
    children = Children(tree,node)
    # add tips
    res = c(res,
            as.character(children[children<=length(tree$tip.label)]))
    for(node in children[children > length(tree$tip.label)]){
      res = c(res,
              cutCladesR(tree,node,tip_states,res))
    }
  }
  return(unique(res))
}

cutClades = function(tree,tip_states){
  x = cutCladesR(tree,length(tree$tip.label)+1,tip_states)
  x2 = sapply(strsplit(x,"-"),as.numeric)
  unlist(sapply(tree$tip.label,function(X){
    which(sapply(x2,function(z){X %in% z}))
  }))
}

simulatePerm = function(n){
  tree =  rcoal(n, br="coalescent")
  
  tree = generate_random_tree(list(birth_rate_intercept=1),max_tips=n)$tree
  
  Nstates = 2
  Q = get_random_mk_transition_matrix(Nstates, rate_model="ARD", max_rate=0.1)
  tip_states = simulate_mk_model(tree, Q)$tip_states
  if(length(unique(tip_states))>1){
  clade = cutClades(tree,tip_states)
  
  
  #plot(tree, show.tip.label = F)
  #tiplabels(tree$tip.label, 
  #          col=tip_states,
  #          bg = sample(rainbow(length(unique(clade))))[as.numeric(as.factor(clade))])
  
  cd = comparative.data(tree,data=
                     data.frame(names=1:length(tree$tip.label),
                                tip_state = tip_states),
                   names.col = "names")
  
  phyloD = phylo.d(cd,tree,binvar=tip_state)

  return(c(
    clades = length(unique(clade)),
    Q = as.vector(Q),
    D = phyloD$DEstimate
           ))
  } else{
    return(c(
      clades=NA,
      Q = NA,
      D = NA
    ))
  }
}

n = 30
numSims = 100
res = replicate(numSims,simulatePerm(n))

res = as.data.frame(do.call(rbind,res))

plot(res$clades,res$D.Obs)
