setwd("***/IOp Mexico") 

# Libraries & Functions----
library(ggparty)
library(mable)
library(lognorm)
library(haven)
library(mlt)
library(DescTools)
library(dineq)
library(tidyverse)

source("./Code/Auxiliar-Functions.R")

# Data ----
df <- read_dta("./Data/Emovi_2017.dta")

df <- df %>% transmute(
  ID = folio, 
  wealth_origin      = as.numeric(percentil_or), 
  wealth_destination = as.numeric(percentil_des),
  region_origin      = recode_factor(as.double(region14),
                                     `1` = "N",
                                     `2` = "NO",
                                     `3` = "CO",
                                     `4` = "C",
                                     `5` = "CDMX",
                                     `6` = "S"),
  parents_educ       = as.double(anesc_padres), 
  area               = recode_factor(rural,
                                     `1` = "R",
                                     `0` = "U"),
  sex                = recode_factor(as.double(sexo_inf),
                                     `2` = "W",
                                     `1` = "M"),
  skin_tone          = recode_factor(as.character(color_p), 
                                     "1" = "D",
                                     "2" = "D", 
                                     "3" = "D",
                                     "4" = "D", 
                                     "5" = "D",
                                     "6" = "D",
                                     "7" = "B",
                                     "8" = "LB",
                                     "9" = "W",
                                     "10" = "W",
                                     "11" = "W"),
  ind_leng_parents   = recode_factor(as.double(hli), 
                                     `1` = "Y",
                                     `0` = "N")
) 

results <- as.data.frame(
  cbind(
    area         = NULL,
    G_XWE_B      = NULL, 
    G_XBE_B      = NULL,
    G_XWE_L      = NULL,
    G_XBE_L      = NULL,
    G_XWE_K      = NULL,
    G_XBE_K      = NULL,
    G_XWT        = NULL,
    G_XBT        = NULL,
    Gini_outcome = NULL)
  )

defaultW <- getOption("warn")
options(warn = -1)

# Estimation of Types ----
trees <- c("2017", "R", "U", "N", "NO", "CO", "C", "CDMX", "S")
depth <- c(9, 6, 6, 4, 5, 7, 7, 5, 6) 
j = 0
for (a in trees){
  j = j+1
  print(a)
  if (a == "2017"){
    df_ <- df
  }
  else if (a == "R" | a == "U") {
    df_ <- df %>% filter(area == a)
  }
  else {
    df_ <- df %>% filter(region_origin == a)
  }
  
  set.seed(123)
  tree             <- ctree(wealth_destination~.-ID,
                            control = ctree_control(maxdepth = depth[j]),
                            data = df_)
  
  df_$predicted    <- predict(tree,
                              data = df_, 
                              type = "node")
  types            <- sort(unique(df_$predicted))
  
  df_ <- df_ %>% transmute(
    ID, wealth_origin, region_origin, area, sex, parents_educ, 
    outcome_destino_cont = as.numeric(wealth_destination),
    nodes                = factor(predicted)
  )
  
  png(paste0("./Plots/", a, "/tree", ".png"),
      width = 7500, height = 600, units = "px", pointsize = 12,
      bg = "white")
  plot(tree, type = "simple")
  dev.off()
  
  plt <- (
    ggparty(
      tree,
      terminal_space = 0,
      add_vars       = list(intercept = "$node$info$nobs",
                            beta      = "$data$wealth_destination")) +
      geom_edge(
        size = .5
      ) +
      geom_edge_label(
        colour = "black",
        size   = 3
      ) +
      geom_node_label(
        aes(col = splitvar),
        line_list = list(aes(label = splitvar),
                         aes(label = paste("p =",
                                           formatC(p.value,
                                                   format = "e",
                                                   digits = 2))),
                         aes(label = ""),
                         aes(label = id)
        ),
        line_gpar = list(list(size = 10),
                         list(size = 8),
                         list(size = 6),
                         list(size      = 5,
                              col       = "black",
                              fontface  = "bold",
                              alignment = "left")
        ),
        ids = "inner"
      ) +
      geom_node_label(
        line_list = list(aes(label = paste("N =", round(intercept, 2))),
                         aes(label = paste("Y =", round(beta, 2))),
                         aes(label = ""),
                         aes(label = id)
        ),
        line_gpar = list(list(size = 10),
                         list(size = 10),
                         list(size = 6),
                         list(size = 5,
                              col = "black",
                              fontface = "bold",
                              alignment = "left")),
        ids     = "terminal",
        nudge_y = -.05
      ) +
      theme(
        legend.position   = "none"
      ) +
      coord_cartesian(
        xlim = c(0, 1),
        ylim = c(-0.1, 1.1)
      ) + 
      labs(color = "Varible:")
  )
  if (a == "2017"){ 
    ggsave(
      paste0("arbol", a, ".png"),
      path    = paste0("./Plots/", a),
      plot    = plt,
      width   = 70,
      height  = 30,
      units   = "cm",
      bg = "white"
    )
  }
  else {
    ggsave(
      paste0("arbol", a, ".png"),
      path    = paste0("./Plots/", a),
      plot    = plt,
      width   = 40,
      height  = 20,
      units   = "cm",
      bg = "white"
    )
  }
  
  # Estimation of Effort
  df_merge = cbind(ID        = NA, 
                   quintil_b = NA, 
                   quintil_l = NA, 
                   quintil_k = NA)
  df_ecdf  <- NULL
  
  for (i in types) {
    x    <- NA
    q_b  <<- NA
    q_l  <<- NA
    q_k  <<- c(NA)
    qi_b <<- c(NA)
    qi_l <<- c(NA)
    qi_k <<- c(NA)
    
    name    <- paste0("df", i)
    dfi     <<- filter(df_,
                       nodes == i)
    assign(name, filter(df_, nodes == i))
    x       <- dfi$outcome_destino_cont
    x       <- replace(x, x == 0, 0.000001)
    df_ecdf <- cbind(df_ecdf, x)
    
    ## Bernstein 
    res <- mable(x,
                 M        = c(2, 100),
                 interval = c(0, max(x)),
                 controls = mable.ctrl(sig.level = 1e-8,
                                       maxit     = 2000,
                                       eps       = 1.0e-9)
    )
    b    <- numeric_var("outcome_destino_cont", "quintil destino",
                        support = c(0, max(x)), 
                        bounds  = c(0, max(x)))
    ctmm <- ctm(response = Bernstein_basis(b, 
                                           order = res$m, 
                                           ui    = "increasing"))
    mltm <- mlt(ctmm, 
                data = dfi,
                coef = res$p)
    q_b  <<- predict(mltm,
                     newdata = dfi, 
                     type    = "quantile",
                     p       = c(.2, .4, .6, .8))
    
    sacaQuitnil(x, 
                distribucion = "bernstein")
    quintil_i_b <- paste0("quintil_b_nodo_", i)
    assign(quintil_i_b, qi_b)
    
    ## Lognormal
    logn <- estimateParmsLognormFromSample(x)
    l    <- plnorm(x,
                   meanlog = logn[1],
                   sdlog   = logn[2])
    q_l  <<- qlnorm(c(.2, .4, .6, .8),
                    meanlog = logn[1], 
                    sdlog   = logn[2])
    
    sacaQuitnil(x,
                distribucion = "log-normal")
    quintil_i_l <- paste0("quintil_l_nodo_", i)
    assign(quintil_i_l, qi_l)
    
    ## Kernell
    k   <- density(x,
                   kernel = "gaussian")
    q_k <<- quantile(k[[1]],
                     c(.2, .4, .6, .8))
    
    sacaQuitnil(x, 
                distribucion = "Kernel")
    quintil_i_k <- paste0("quintil_k_nodo_", i)
    assign(quintil_i_k, qi_k)
    
    # Histogramas
    png(paste0("./Plots/", a, "/hist_", i, ".png"))
    ## Histogram
    hist(x,
         prob   = T,
         main   = paste("Histogram and density for type (node)", i),
         border = "dark grey",
         lwd    = 1,
         xlab = "x",
         ylab = "f(x)", 
         col = "light grey",
         ylim = c(0, 0.05),
         xlim = c(0, max(x)+0.25)
    )
    ## Kernell Distribution
    lines(k, 
          lty = 4,
          col = 4)
    ## Normal Distribution
    lines(y <- seq(0, 100,
                   length = length(x)),
          dlnorm(y,
                 mean = logn[1],
                 sd   = logn[2]), 
          lty = 2, 
          col = 2)
    ## Bersntein
    plot(res, 
         which = "density", 
         add   = TRUE)
    
    legend(
      "topleft",
      lty = c(1, 2, 4),
      col = c(1, 2, 4),
      bty = "n",
      c("Bernstein", "Log-Normal", "Kernel-Gaussiano")
    )
    dev.off()
    
    df_quintil_i <- as.data.frame(cbind(ID        = dfi$ID,
                                        quintil_b = qi_b, 
                                        quintil_l = qi_l,
                                        quintil_k = qi_k))
    df_merge     <- rbind(df_merge, df_quintil_i)
  }
  
  df_para_merge = as.data.frame(df_merge)
  df_final      = full_join(df_, df_merge, 
                            by = 'ID')
  
  # ECDF ----
  df_ecdf <- as.data.frame(df_ecdf)
  names(df_ecdf) <- types
  
  x <- NULL
  
  for (i in colnames(df_ecdf)) {
    x <- append(x, df_ecdf[, i])
  }
  
  df_ecdf1 <- data.frame(x,
                         g = gl(ncol(df_ecdf),
                                nrow(df_ecdf))
  )
  
  
  dens     <- split(df_ecdf1,
                    df_ecdf1$g) %>%
    map_df(function(d) {
      dens <- density(d$x,
                      from = 0)
      data.frame(x  = dens$x, 
                 y  = dens$y,
                 cd = cumsum(dens$y)/sum(dens$y), 
                 g  = d$g[1])
    })
  
  i = 1
  for (g in dens$g) {
    dens$t[i] <- types[as.double(g)]
    
    i = i+1
  }
  
  plt_ecdf <- ggplot(data = dens) + aes(x      = x,
                                        y      = cd, 
                                        colour = factor(t)) +
    geom_line() +
    labs(x     = 'Outcome',
         y     = "ECDF", 
         color = "Type group") +
    xlim(1,100) +
    ggthemes::theme_base()
  
  ggsave(
    paste0("ECDF_", a, ".png"),
    path   = paste0("./Plots/", a),
    plot   = plt_ecdf,
    width  = 40,
    height = 20,
    units  = "cm"
  )
  
  # IOP Effort Approach
  df_final <- df_final %>%
    mutate(
      pop_mean_outcome = mean(outcome_destino_cont,
                              na.rm = T)
    )
  
  ## Bernstein
  b <- df_final %>%
    group_by(quintil_b) %>%
    summarise(
      qesf_mean_outcome = mean(outcome_destino_cont,
                               na.rm = T)
    )
  
  new_base <- left_join(df_final, b,
                        by = "quintil_b")
  new_base <- new_base %>%
    mutate(
      a_b            = pop_mean_outcome/qesf_mean_outcome,
      outcome_expost = outcome_destino_cont*a_b,
      XWE_bernstein  = outcome_expost,
      XBE_bernstein  = qesf_mean_outcome,
    )
  
  ## Lognormal
  b_l <- df_final %>%
    group_by(quintil_l) %>%
    summarise(
      qesf_mean_outcome_l = mean(outcome_destino_cont, 
                                 na.rm = T)
    )
  
  new_base <- merge(new_base, b_l,
                    by = "quintil_l") %>%
    mutate(
      a_b_log          = pop_mean_outcome/qesf_mean_outcome,
      outcome_expost_l = outcome_destino_cont*a_b_log,
      XWE_Lognormal    = outcome_expost_l,
      XBE_Lognormal    = qesf_mean_outcome_l,
    )
  
  ## Kernel
  b_k <- df_final %>%
    group_by(quintil_k) %>%
    summarise(
      qesf_mean_outcome_k = mean(outcome_destino_cont,
                                 na.rm = T)
    )
  
  new_base <- merge(new_base, b_k,
                    by = "quintil_k") %>%
    mutate(
      a_b_kernel       = pop_mean_outcome/qesf_mean_outcome,
      outcome_expost_k = outcome_destino_cont*a_b_kernel,
      XWE_kernel       = outcome_expost_k,
      XBE_kernel       = qesf_mean_outcome_k,
    )
  
  
  # Types Approach ----
  bt <- new_base %>%
    group_by(nodes) %>%
    summarise(
      type_mean_outcome = mean(outcome_destino_cont,
                               na.rm = T)
    )
  
  new_base2 <- merge(new_base, bt,
                     by = "nodes") %>% 
    mutate(
      XBT  = type_mean_outcome,
      a_bt = pop_mean_outcome/type_mean_outcome,
      XWT  = outcome_destino_cont*a_bt
    )
  
  
  XWE_bernstein <- new_base2$XWE_bernstein
  XBE_bernstein <- new_base2$XBE_bernstein
  XWE_Lognormal <- new_base2$XWE_Lognormal
  XBE_Lognormal <- new_base2$XBE_Lognormal
  XWE_Kernel    <- new_base2$XWE_kernel
  XBE_Kernel    <- new_base2$XBE_kernel
  
  XWT <- new_base2$XWT
  XBT <- new_base2$XBT
  
  outcome_continua <- new_base2$outcome_destino_cont
  
  # Gini----
  G_XWE_B <- Gini(XWE_bernstein, 
                  na.rm = T)
  G_XBE_B <- Gini(XBE_bernstein,
                  na.rm = T)
  G_XWE_L <- Gini(XWE_Lognormal, 
                  na.rm = T)
  G_XBE_L <- Gini(XBE_Lognormal,
                  na.rm = T)
  G_XWE_K <- Gini(XWE_Kernel, 
                  na.rm = T)
  G_XBE_K <- Gini(XBE_Kernel, 
                  na.rm = T)
  
  G_XWT <- Gini(XWT, 
                na.rm = T)
  G_XBT <- Gini(XBT, 
                na.rm = T)
  
  Gini_outcome <- Gini(outcome_continua, na.rm = T)
  
  
  # MLD ----
  M_XWE_B <- mld.wtd(XWE_bernstein)
  M_XBE_B <- mld.wtd(XBE_bernstein)
  M_XWE_L <- mld.wtd(XWE_Lognormal)
  M_XBE_L <- mld.wtd(XBE_Lognormal)
  M_XWE_K <- mld.wtd(XWE_Kernel)
  M_XBE_K <- mld.wtd(XBE_Kernel)
  
  M_XWT <- mld.wtd(XWT)
  M_XBT <- mld.wtd(XBT)
  
  MLD_outcome <- mld.wtd(outcome_continua)
  

  results_a <- as.data.frame(cbind(area = a,
                                   G_XWE_B, G_XBE_B, G_XWE_L,
                                   G_XBE_L, G_XWE_K, G_XBE_K,
                                   G_XWT, G_XBT, Gini_outcome,
                                   M_XWE_B, M_XBE_B, M_XWE_L,
                                   M_XBE_L, M_XWE_K, M_XBE_K,
                                   M_XWT, M_XBT, MLD_outcome)
  )
  results   <- rbind(results, results_a)
}

write.csv(results, './Results/Results.csv')
