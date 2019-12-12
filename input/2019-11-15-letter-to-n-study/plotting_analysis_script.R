
library(entropy)
library(igraph)
library(digest)
library(scatterplot3d)
library(ggplot2)
library(plyr)
library(tidyr)
library(tidyverse)
library(jtools)

dirs <- list.dirs("~/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study/",full.names = T,recursive = F)
dirs <- dirs[-which(dirs=="/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/Git/transcendental-information-cascades/output/2019-11-15-letter-to-n-study//postProcessTICMultiplex")]
#dirs <- dirs[21]
for(di in dirs){
  dir.create(file.path(paste0(di), "figures"), showWarnings = FALSE)
  #any links?
  if(file.info(paste0(di,"/createTIC/links.csv"))$size > 0){
    # data we need
    nodes <- read.csv(paste0(di,"/createTIC/nodes.csv"),stringsAsFactors = F)
    links <- read.csv(paste0(di,"/createTIC/links.csv"), stringsAsFactors = F)
    
    ###powerlaw of link distance???
    library(poweRlaw)
    
    
    linksDelta <- as.integer(links$target)-as.integer(links$source)
    jpeg(paste0(di,"/figures/links_delta.jpg"))
    plot(linksDelta,type='l')
    abline(h=mean(linksDelta),col="red")
    abline(h=median(linksDelta),col="blue")
    dev.off()
    linksDelta.count <- plyr::count(linksDelta)
    jpeg(paste0(di,"/figures/links_delta_distri.jpg"))
    plot(linksDelta.count$x,linksDelta.count$freq,pch=20)
    dev.off()
    
    jpeg(paste0(di,"/figures/links_delta_distri_loglog.jpg"))
    plot(linksDelta.count$x,linksDelta.count$freq,pch=20,log="xy")
    dev.off()
    
    #power law?
    
    #powerlaw
    m_bl = displ$new(linksDelta)
    est = estimate_xmin(m_bl)
    #m_bl$setXmin(est)
    m_bl$setPars(estimate_pars(m_bl))
    #lognormal
    m_ln = dislnorm$new(linksDelta)
    est = estimate_xmin(m_ln)
    #m_ln$setXmin(est)
    m_ln$setPars(estimate_pars(m_ln))
    #poissoin
    m_pois = dispois$new(linksDelta)
    est = estimate_xmin(m_pois)
    #m_pois$setXmin(est)
    m_pois$setPars(estimate_pars(m_pois))
    
    jpeg(paste0(di,"/figures/links_delta_distri_plaw.jpg"))
    plot(m_bl, ylab="CDF",pch=20)
    #text(100,0.15,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
    lines(m_bl, col=2)
    lines(m_ln, col=3)
    lines(m_pois, col=4)
    dev.off()
    
    #powerlaw of term recurrence?
    recurs <- plyr::count(links$token)$freq
    if(length(recurs)>1){
      #powerlaw
      m_bl = displ$new(recurs)
      est = estimate_xmin(m_bl)
      #m_bl$setXmin(est)
      m_bl$setPars(estimate_pars(m_bl))
      #lognormal
      m_ln = dislnorm$new(recurs)
      est = estimate_xmin(m_ln)
      #m_ln$setXmin(est)
      m_ln$setPars(estimate_pars(m_ln))
      #poissoin
      m_pois = dispois$new(recurs)
      est = estimate_xmin(m_pois)
      #m_pois$setXmin(est)
      m_pois$setPars(estimate_pars(m_pois))
      
      jpeg(paste0(di,"/figures/recurrence_distri_plaw.jpg"))
      plot(m_bl, ylab="CDF",pch=20)
      #text(100,0.15,bquote(x[min] ~ .(paste0("=")) ~ .(m_bl$xmin) ~ .(paste0(", ")) ~ alpha ~ .(paste0("=")) ~ .(m_bl$pars)))
      lines(m_bl, col=2)
      lines(m_ln, col=3)
      lines(m_pois, col=4)
      dev.off()
    }
    
    
    #struct <- read.csv(paste0(di,"/createTIC/TICStructFeatures.csv"),stringsAsFactors = F)
    #ent <- read.csv(paste0(di,"/createTIC/TICInfoTheory1.csv"),stringsAsFactors = F)
    wien <- read.csv(paste0(di,"/createTIC/TICInfoTheory2.csv"),stringsAsFactors = F)
    coordinates <- read.csv(paste0(di,"/createTIC/TICCoordinates.csv"),stringsAsFactors = F)
    
    wien_plot <- as.data.frame(wien) %>%
      mutate(rownumber = seq.int(nrow(.)))
    
    # struc_plot <- as.data.frame(struct) %>%
    #   mutate(rownumber = seq.int(nrow(.)))
    
    ggsave(paste0(di,"/figures/shannonwiener.jpg"),
           wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = ShannonWiener, group = 1) +
             geom_line() +
             labs(y = "Shannon Wiener Index") +
             theme_apa())
    
    ggsave(paste0(di,"/figures/pielou.jpg"),
           wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = Pielou, group = 1) +
             geom_line() +
             labs(y = "Pielou Evenness") +
             theme_apa())
    
    ggsave(paste0(di,"/figures/richness.jpg"),
           wien_plot %>%
             ggplot() +
             aes(x = rownumber, y = Richness, group = 1) +
             geom_line() +
             labs(y = "Richness") +
             theme_apa())
    
    # ggsave(paste0(di,"/figures/diameter.jpg"),
    # struc_plot %>%
    #   ggplot() +
    #   aes(x = rownumber, y = diameter, group = 1) +
    #   geom_line() +
    #   labs(y = "Diameter") +
    #   theme_classic())
    # 
    # ggsave(paste0(di,"/figures/density.jpg"),
    # struc_plot %>%
    #   ggplot() +
    #   aes(x = rownumber, y = density, group = 1) +
    #   geom_line() +
    #   labs(y = "Density") +
    #   theme_classic())
    # 
    # ggsave(paste0(di,"/figures/modularity.jpg"),
    # struc_plot %>%
    #   ggplot() +
    #   aes(x = rownumber, y = modularity, group = 1) +
    #   geom_line() +
    #   labs(y = "Modularity") +
    #   theme_classic())
    
    jpeg(paste0(di,"/figures/coordinates.jpg"))
    scatterplot3d(coordinates[,2],coordinates[,1],coordinates[,3],pch=16, highlight.3d=TRUE,type="h",xlab="Specificity",ylab="Node index",zlab="Diversity")
    dev.off()
  }
}

