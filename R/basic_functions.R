#' Life-history parameters
#'

#Function to generate a table of species and its life history parameters
#' @param df2 ifish postgresQL data frame containing fish species, length, and biological parameters
#'
lhparam <- function(df2){
  #remove blank genus and species row
  df2 <- df2[!(df2$fish_genus==""), ]
  df2 <- df2  %>%setNames(make.unique(names(.))) %>%
    dplyr::filter(program_type=='Snapper')
  #Life history parameters
  LHparam <- df2 %>% setNames(make.unique(names(.))) %>%
    unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
    distinct(fishname, .keep_all = TRUE) %>%
    dplyr::select(fishname, cm, lmat, lopt, linf, lmax)
  return(LHparam)}

#'Number of vessels per fishing gear
#'@param df2 ifish postgresQL data frame
#'
#Number of captains per fishing gear
df2cap <- function(df2){
  df2 <- df2 %>% distinct (boat_code, .keep_all = TRUE) %>% group_by(fishing_gear) %>% dplyr::summarize(n())
  return(df2)}

#'Generate table of the top species in the catch by frequency
#'@param df2 ifish postgresQL data frame
#'
#Function to generate a table of the top species in the catch (by frequency)
species_freq <- function(df2){
  by_freq <- df2 %>% setNames(make.unique(names(.))) %>%
    group_by(fishname) %>%
    dplyr::summarize(n()) %>%
    arrange(desc(`n()`))
  return(by_freq)}

#' Generate table for the top species in the catch by frequency and by fishing gear
#' @param df2 ifish postgresQL data frame
#'
#By different fishing gear
species_freq_gear <- function(df2){
  by_gear <- df2 %>% setNames(make.unique(names(.))) %>%
    unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
    # filter(fishery_type=='Snapper') %>%
    group_by(fishing_gear, fishname) %>%
    dplyr::summarize(n()) %>%
    dplyr::arrange(desc(`n()`))
  return(by_gear)}

#' Generate table for the top species in the catch by weight
#' @param df2 ifish postgresQL data frame
#Check the top species in the catch (by weight)
species_biomass <-function(df2) {
  biomass <-  df2 %>% setNames(make.unique(names(.))) %>%
    unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
    mutate(weight=(var_a *(cm^var_b)/1000)) %>%
    group_by(fishname)%>%
    summarise(sum(weight))%>%
    arrange(desc(`sum(weight)`))
  return(biomass)}

#' Generate table for the top species in the catch by weight and fishing gear
#' @param df2 ifish postgresQL data frame
#By different fishing gear
species_biomass_gear <- function(df2){
  biomass_gear <- df2 %>% setNames(make.unique(names(.))) %>%
    unite(fishname, fish_genus, fish_species, sep=" ", remove= FALSE) %>%
    #filter(fishery_type=='Snapper') %>%
    mutate(weight=(var_a *(cm^var_b)/1000)) %>%
    group_by(fishing_gear, fishname) %>%
    dplyr::summarize(sum(weight)) %>%
    dplyr::arrange(desc(`sum(weight)`))}
