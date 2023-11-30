#' @export
extract_siape_entrevistadores <- function(z, j) {
  regex_entrevistador <- "([0-9]+)[^\\|]*\\|*([0-9]+)*.*"#gsub(, "\\2", )
  gsub(x = z, replacement = paste0("\\",j), pattern=regex_entrevistador)%>%dplyr::na_if("")
}


#' example
#' m <- structure(list(siapes_1 = c("1177818 - TASSIA SILVA DOS REIS ", "1177818 - TASSIA SILVA DOS REIS |1295180 - HUMBERTO RODRIGO SILVA E SILVA"), siapes_2 = c("1177818 - TASSIA SILVA DOS REIS |1295180 - HUMBERTO RODRIGO SILVA E SILVA", "1295180 - HUMBERTO RODRIGO SILVA E SILVA|1177818 - TASSIA SILVA DOS REIS")), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))
#' lc_entrevistador(m, siapes_1)
#'
#' @export
# lc_entrevistador <- function(data, var) {
#   data <- dplyr::mutate(data,
#                         v_1 = extract_siape_entrevistadores({{ var }}, 1),
#                         v_2 = extract_siape_entrevistadores({{ var }}, 2)
#   ) %>% select(-{{ var }})
#   data %>%
#     rowwise() %>%
#     mutate("{{var}}" := list(c(v_1, v_2) %>% na.omit())) %>%
#     select(-v_1, -v_2)
# }
lc_entrevistador <- function(data, var) {
  tidyr::separate_longer_delim(data, {{var}}, delim="|")
}

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

  # nome_siape_um <- um_rec_uf%>%
  #   distinct(entrevistador)%>%
  #   tidyr::separate_wider_delim(entrevistador, delim=" - ", names=c("siape", "nome"), too_few = "align_start")
  #
  # nome_siape_selecionado <- entrevistadores_selecionados_0%>%
  #   distinct(entrevistador)
  # %>%
  #   tidyr::separate_wider_delim(entrevistador, delim=" - ", names=c("siape", "nome"), too_few = "align_start")


  entrevistadores_status <- entrevistadores_nao_selecionados_0%>%
    bind_rows(entrevistadores_selecionados_0)#%>%left_join(rh_siape%>%select(entrevistador_associado=siape, sexo_entrevistador_associado=sexo))

  stopifnot((entrevistadores_status%>%ungroup%>%distinct(controle, domicilio)%>%nrow)==nrow(um_rec_uf))

  # nome_siape <- entrevistadores_status%>%
  #   reframe(entrevistador=union(entrevistador_associado_siape, entrevistador_selecionado))%>%
  #   bind_rows(entrevistadores_quadro%>%select(entrevistador))%>%
  #   mutate(entrevistador=trimws(entrevistador))%>%
  #   tidyr::separate_wider_delim(entrevistador, names=c("entrevistador_siape", "entrevistador_nome"), delim=" - ",
  #                               too_few="align_start")%>%
  #   mutate(across(everything(), ~tidyr::replace_na(.x, "Sem informação")))%>%
  #   unique

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
    # transmute(#assistencia_nome,
    #   Agência=agencia_nome, Siape=entrevistador,
    #   #Nome=coalesce(nome, nome_siape),
    #   Nome=nome,
    #   #Sexo=sexo,
    #   #Cargo=cargo_nome, Email=email,
    #   # não colocar domicílios total para não confundir: não é o número de associados, nem soma para o total de domicílios na unidade
    #   #`Domicílios Total`=domicilios_total,
    #   `Quadro de morador preenchido`=quadros,
    #   `Selecionado Respondido`,
    #           "Quadro preenchido + Selecionado Respondido"=quadro_mais_selecionado, "Recusa de selecionado ou do Domicílio"=Recusa+`Selecionado Recusa`, `Sem informação`,
    #           pick(any_of(c(
    #             "Selecionado Incapaz",
    #             "Selecionado em andamento",
    #             "Coletivo/obra/ruína/demolido/não residencial", "Não encontrado/fora do setor", "Ocasional/Vago", "Outro motivo"))))
    #
  return(entrevistadores_coleta_export)
  #entrevistadores_coleta_export_list <- entrevistadores_coleta_export%>%split(.$assistencia_nome)%>%purrr::map(~select(.x,-assistencia_nome)%>%janitor::adorn_totals(name = "TOTAL"))

  #readr::write_rds(entrevistadores_coleta_export_list, "data/entrevistadores_coleta_export_list.rds")

  #excel(entrevistadores_coleta_export_list, filename = "results/producao_entrevistador.xlsx", firstrow = paste0("Relatório: ", format(lubridate::with_tz(um_rec_uf$relatorio_data_hora[1], tz=""), "%d/%m/%y %R")), open=FALSE)
}
