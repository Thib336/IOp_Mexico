sacaQuitnil = function(x, distribucion = "bernstein") {
  if (distribucion == "bernstein") {
    for (j in 1:length(x)){
      if (dfi$outcome_destino_cont[j] <= q_b[1]){
        qi_b[j] <<- 1L
      }
      else if (dfi$outcome_destino_cont[j] <= q_b[2] & dfi$outcome_destino_cont[j] > q_b[1]) {
        qi_b[j] <<- 2L
      }
      else if (dfi$outcome_destino_cont[j] <= q_b[3] & dfi$outcome_destino_cont[j] > q_b[2]) {
        qi_b[j] <<- 3L
      }
      else if (dfi$outcome_destino_cont[j] <= q_b[4] & dfi$outcome_destino_cont[j] > q_b[3]) {
        qi_b[j] <<- 4L
      }
      else if (dfi$outcome_destino_cont[j] > q_b[4]) {
        qi_b[j] <<- 5L
      }
    }
    return(qi_b)
  }
  
  else if (distribucion == "log-normal") {
    for (j in (seq_len(nrow(dfi)))){
      if (dfi$outcome_destino_cont[j] <= q_l[1]){
        qi_l[j] <<- 1L
      }
      else if (dfi$outcome_destino_cont[j] <= q_l[2] & dfi$outcome_destino_cont[j] > q_l[1]) {
        qi_l[j] <<- 2L
      }
      else if (dfi$outcome_destino_cont[j] <= q_l[3] & dfi$outcome_destino_cont[j] > q_l[2]) {
        qi_l[j] <<- 3L
      }
      else if (dfi$outcome_destino_cont[j] <= q_l[4] & dfi$outcome_destino_cont[j] > q_l[3]) {
        qi_l[j] <<- 4L
      }
      else if (dfi$outcome_destino_cont[j] > q_l[4]) {
        qi_l[j] <<- 5L
      }
    }
    return(qi_l)
  }
  
  else {
    for (j in (seq_len(nrow(dfi)))){
      if (dfi$outcome_destino_cont[j] <= q_k[1]){
        qi_k[j] <<- 1L
      }
      else if (dfi$outcome_destino_cont[j] <= q_k[2] & dfi$outcome_destino_cont[j] > q_k[1]) {
        qi_k[j] <<- 2L
      }
      else if (dfi$outcome_destino_cont[j] <= q_k[3] & dfi$outcome_destino_cont[j] > q_k[2]) {
        qi_k[j] <<- 3L
      }
      else if (dfi$outcome_destino_cont[j] <= q_k[4] & dfi$outcome_destino_cont[j] > q_k[3]) {
        qi_k[j] <<- 4L
      }
      else if (dfi$outcome_destino_cont[j] > q_k[4]) {
        qi_k[j] <<- 5L
      }
    }
    return(qi_k)
  }
}

pegarQuintiles = function() {
  
  df5$quintil_b = quintil_b_nodo_5
  df6$quintil_b = quintil_b_nodo_6
  df9$quintil_b = quintil_b_nodo_9
  df10$quintil_b = quintil_b_nodo_10
  df12$quintil_b = quintil_b_nodo_12
  df13$quintil_b = quintil_b_nodo_13
  df15$quintil_b = quintil_b_nodo_15
  df17$quintil_b = quintil_b_nodo_17
  df18$quintil_b = quintil_b_nodo_18
  df22$quintil_b = quintil_b_nodo_22
  df24$quintil_b = quintil_b_nodo_24
  df25$quintil_b = quintil_b_nodo_25
  df28$quintil_b = quintil_b_nodo_28
  df29$quintil_b = quintil_b_nodo_29
  df31$quintil_b = quintil_b_nodo_31
  df32$quintil_b = quintil_b_nodo_32
  df36$quintil_b = quintil_b_nodo_36
  df37$quintil_b = quintil_b_nodo_37
  df39$quintil_b = quintil_b_nodo_39
  df40$quintil_b = quintil_b_nodo_40
  df42$quintil_b = quintil_b_nodo_42
  df44$quintil_b = quintil_b_nodo_44
  df45$quintil_b = quintil_b_nodo_45

  df5$quintil_l = quintil_l_nodo_5
  df6$quintil_l = quintil_l_nodo_6
  df9$quintil_l = quintil_l_nodo_9
  df10$quintil_l = quintil_l_nodo_10
  df12$quintil_l = quintil_l_nodo_12
  df13$quintil_l = quintil_l_nodo_13
  df15$quintil_l = quintil_l_nodo_15
  df17$quintil_l = quintil_l_nodo_17
  df18$quintil_l = quintil_l_nodo_18
  df22$quintil_l = quintil_l_nodo_22
  df24$quintil_l = quintil_l_nodo_24
  df25$quintil_l = quintil_l_nodo_25
  df28$quintil_l = quintil_l_nodo_28
  df29$quintil_l = quintil_l_nodo_29
  df31$quintil_l = quintil_l_nodo_31
  df32$quintil_l = quintil_l_nodo_32
  df36$quintil_l = quintil_l_nodo_36
  df37$quintil_l = quintil_l_nodo_37
  df39$quintil_l = quintil_l_nodo_39
  df40$quintil_l = quintil_l_nodo_40
  df42$quintil_l = quintil_l_nodo_42
  df44$quintil_l = quintil_l_nodo_44
  df45$quintil_l = quintil_l_nodo_45
  
  df5$quintil_k = quintil_k_nodo_5
  df6$quintil_k = quintil_k_nodo_6
  df9$quintil_k = quintil_k_nodo_9
  df10$quintil_k = quintil_k_nodo_10
  df12$quintil_k = quintil_k_nodo_12
  df13$quintil_k = quintil_k_nodo_13
  df15$quintil_k = quintil_k_nodo_15
  df17$quintil_k = quintil_k_nodo_17
  df18$quintil_k = quintil_k_nodo_18
  df22$quintil_k = quintil_k_nodo_22
  df24$quintil_k = quintil_k_nodo_24
  df25$quintil_k= quintil_k_nodo_25
  df28$quintil_k = quintil_k_nodo_28
  df29$quintil_k = quintil_k_nodo_29
  df31$quintil_k = quintil_k_nodo_31
  df32$quintil_k = quintil_k_nodo_32
  df36$quintil_k = quintil_k_nodo_36
  df37$quintil_k = quintil_k_nodo_37
  df39$quintil_k = quintil_k_nodo_39
  df40$quintil_k = quintil_k_nodo_40
  df42$quintil_k = quintil_k_nodo_42
  df44$quintil_k = quintil_k_nodo_44
  df45$quintil_k = quintil_k_nodo_45
  
  df_quintil <<- rbind(df5, df6,  df9, df10, df12, df13, df15, df17, df18, df22, df24, df25, df28, df29, df31, df32, df36, df37, df39, df40, df42, df44, df45)
  

}

