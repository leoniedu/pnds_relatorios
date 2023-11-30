#' @export
#' @export
separate_entrevistador <- function(data,cols, names, delim_wide=" - ",
                                   delim_long="|",
                                   replace_na="Sem informação", too_few = "align_start") {
  data <- tidyr::separate_longer_delim(data, {{cols}}, delim=delim_long)
  res <- data%>%tidyr::separate_wider_delim(cols={{cols}}, names=names, too_few = "align_start", delim=delim_wide)
  res%>%mutate(across(all_of(names), function(x) trimws(x)%>%
                        dplyr::na_if("")%>%
                        tidyr::replace_na(replace_na)))
}


#' export
producao_entrevistador <- function(ultimo_movimento, gerencial_compilado_1) {
  require(dplyr)
  um_rec_uf <- ultimo_movimento%>%
    janitor::clean_names()%>%
    separate_entrevistador(entrevistador, names = c("entrevistador_associado_siape","entrevistador_associado_nome"))%>%
    tidyr::separate_wider_delim(agencia, delim=" - ", names = c("agencia_codigo", 'agencia_nome'))


  #por domicilio/controle/morador selecionado
  gerencial_uf <- gerencial_compilado_1 %>% janitor::clean_names() %>%
    collect()

  u_sel <- um_rec_uf %>%
    filter(tipo_entrevista=="Realizada")%>%
    group_by(controle, domicilio)%>%
    mutate(sexo_entrevistado=list(c(if_else(status_morador_selecionado!="Sem informação", "M", NA_character_), if_else(status_moradora_selecionada!="Sem informação", "F", NA_character_))%>%na.omit))%>%
    tidyr::unnest(sexo_entrevistado)

  g0 <- gerencial_uf%>%
    filter((morador_selecionado=="Sim") | (moradora_selecionada=="Sim"))%>%
    mutate(sexo_entrevistado=if_else(morador_selecionado=="Sim", "M", "F"))%>%
    rename(entrevistador_selecionado=siape_s_moradores_selecionados)%>%
    separate_entrevistador(entrevistador_selecionado, names = c("entrevistador_selecionado_siape", "entrevistador_selecionado_nome"))%>%
    ungroup


  entrevistadores_selecionados_0 <- u_sel%>%
    left_join(g0, by=c("controle", "domicilio", "sexo_entrevistado"))%>%
    group_by(controle, domicilio, sexo_entrevistado)%>%
    mutate(w_selecionado=1/n())%>%
    #mutate(id=as.numeric(entrevistador_selecionado))%>%
    # left_join(rh_siape%>%transmute(id=as.numeric(siape), sexo_entrevistador_selecionado=sexo), by=c("id"))%>%
    mutate(status_selecionado=
             paste("Selecionado", tidyr::replace_na(status_quest_morador_a_selecionado_a, "em andamento"
             )))%>%
    distinct(controle, domicilio, status_selecionado, entrevistador_associado_siape, entrevistador_associado_nome, entrevistador_selecionado_nome, entrevistador_selecionado_siape, sexo_entrevistado
             #, sexo_entrevistador_selecionado,
             ,tipo_entrevista, #tipo_entrevista_rec,
             status_quadro_moradores, status_nucleo_basico, w_selecionado)



  entrevistadores_nao_selecionados_0 <- um_rec_uf%>%
    anti_join(entrevistadores_selecionados_0, by=c("controle", "domicilio"))%>%
    transmute(controle, domicilio, entrevistador_associado_nome, entrevistador_associado_siape, tipo_entrevista, #tipo_entrevista_rec,
              status_selecionado=case_when(
                (status_moradora_selecionada=="Sem informação") & (status_morador_selecionado=="Sem informação") & (tipo_entrevista=="Realizada") ~ "Realizada sem selecionados",
                (tipo_entrevista=="") | is.na(tipo_entrevista) ~ "Sem informação",
                #TRUE ~ tipo_entrevista_rec
                TRUE ~ tipo_entrevista
                ))

  entrevistadores_quadro <- gerencial_uf %>%
    left_join(um_rec_uf, by=c("controle", "domicilio"), suffix = c("_g", ''))%>%
    filter(tipo_entrevista=="Realizada")%>%
    separate_entrevistador(siape_s_quadro_moradores,names = c("entrevistador_quadro_siape", "entrevistador_quadro_nome"))%>%
    distinct(entrevistador_quadro_siape, entrevistador_quadro_nome, controle, agencia_codigo, domicilio)%>%
    group_by(controle, domicilio)%>%
    mutate(w_quadro=1/n())%>%
    group_by(entrevistador_quadro_siape, entrevistador_quadro_nome, agencia_codigo)%>%
    summarise(n_quadro_entrevistador=sum(w_quadro))%>%
    ungroup

  entrevistadores_status <- entrevistadores_nao_selecionados_0%>%
    bind_rows(entrevistadores_selecionados_0)

  stopifnot((entrevistadores_status%>%ungroup%>%distinct(controle, domicilio)%>%nrow)==nrow(um_rec_uf))

  entrevistadores_coleta <- entrevistadores_status%>%
    mutate(entrevistador_siape=coalesce(entrevistador_selecionado_siape, entrevistador_associado_siape),
           entrevistador_nome=coalesce(entrevistador_selecionado_nome, entrevistador_associado_nome)
           )%>%
    left_join(um_rec_uf%>%distinct(controle, agencia_codigo), by=c('controle')) %>%
    group_by(entrevistador_siape, entrevistador_nome, agencia_codigo)%>%
    mutate(n_domicilios=n())%>%
    group_by(entrevistador_siape, entrevistador_nome, agencia_codigo, n_domicilios, status_selecionado)%>%
    summarise(n_selecionados=sum(w_selecionado%>%tidyr::replace_na(1), na.rm=TRUE))%>%
    ungroup%>%
    mutate(status_selecionado=forcats::fct_infreq(status_selecionado, ordered = TRUE))%>%
    arrange(status_selecionado)%>%
    full_join(entrevistadores_quadro, by=c("entrevistador_siape"="entrevistador_quadro_siape", "agencia_codigo"))%>%
    mutate(across(where(is.numeric), function(x) tidyr::replace_na(x, 0)))%>%
    rename(quadros=n_quadro_entrevistador, domicilios_total=n_domicilios)%>%
    tidyr::pivot_wider(id_cols = c(agencia_codigo, entrevistador_siape, entrevistador_nome, domicilios_total, quadros), names_from = status_selecionado, values_from=n_selecionados, values_fill = 0)%>%
    mutate("quadro_mais_selecionado"=quadros+`Selecionado Respondido`)%>%
    left_join(um_rec_uf%>%distinct(agencia_codigo, agencia_nome))#%>%

  entrevistadores_coleta_export <- entrevistadores_coleta%>%
    arrange(agencia_nome, desc(quadro_mais_selecionado))%>%
    select(agencia_codigo, agencia_nome, entrevistador_siape, entrevistador_nome, quadro_mais_selecionado,  everything())
  return(entrevistadores_coleta_export)
}
