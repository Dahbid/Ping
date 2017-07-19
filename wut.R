##### inlezen en libraries #####
pingpong <- readxl::read_xlsx("pingpong.xlsx")
pingpong <- as.data.frame(pingpong)

library(dplyr)
library(reshape2)
library(plotly)

##### Functies #####
winnen <- function(test, type) {
  type2 <- names(test[grepl(type, names(test))])
  spelers <- unique( c(unique(test[, type2[1]]), unique(test[, type2[2]])) )
  variabelen <- c("Datum", type2)
  
  test <- test[, variabelen]
  
  for (i in spelers) {
    test[, i] <- 0
    test[, i] <- ifelse(test[, type2[1]] == i, test[, i] + 1, test[, i])
    test[, i] <- ifelse(test[, type2[2]] == i, test[, i] + 1, test[, i])
    test[, i] <- cumsum(test[, i])
  }
  
  test <- test[, c("Datum", spelers)]
  test <- reshape2::melt(test, id.vars = "Datum")
  return(test)
}

win <- function(test, type) {
  spelers <- unique(test[[type]])
  variabelen <- c("Datum", type)
  
  test <- test[, variabelen]
  for (i in spelers) {
    test[, i] <- 0
    test[, i] <- ifelse(test[, type] == i, test[, i] + 1, test[, i])
    test[, i] <- cumsum(test[, i])
  }
  test <- test[, c("Datum", spelers)]
  test <- reshape2::melt(test, id.vars = "Datum")
  
  return(test)
}

meedoen <- function(test) {
  type <- names(test[!grepl('Datum', names(test))])
  spelers <- unique(unlist(lapply(test[, type], unique)))
  
  for (i in spelers) {
    test[, i] <- 0
    test[, i] <- ifelse(test[, type[1]] == i, test[, i] + 1, test[, i])
    test[, i] <- ifelse(test[, type[2]] == i, test[, i] + 1, test[, i])
    test[, i] <- ifelse(test[, type[3]] == i, test[, i] + 1, test[, i])
    test[, i] <- ifelse(test[, type[4]] == i, test[, i] + 1, test[, i])
    test[, i] <- cumsum(test[, i])
  }
  
  test <- test[, c("Datum", spelers)]
  test <- reshape2::melt(test, id.vars = "Datum")
  return(test)
}

##### Analyse #####

##### Tijdlijngrafieken #####
# Winnaars en verliezers
alle_winnaars <- winnen(pingpong, "Winnaar")
alle_verliezers <- winnen(pingpong, "Verliezer")
iedereen <- meedoen(pingpong)
winnaar_usopen <- win(pingpong, "Winnaar.US.Open")
winnaar_wimbledon <- win(pingpong, "Winnaar.Wimbledon")
# base <- alle_winnaars %>%
#   plot_ly(x = ~nummer, y = ~value, color = ~variable) 
# 
# plot_ly(x = ~Datum, y = ~value, color = ~variable, data = alle_winnaars)

plot_ly(data = alle_winnaars, x = ~Datum, y = ~value, color = ~variable, type = 'scatter', mode = 'lines') %>%
  add_markers(y = ~value, showlegend = FALSE)

plot_ly(data = alle_winnaars, x = ~as.character(Datum), y = ~value, color = ~variable, type = 'scatter', mode = 'lines') %>%
  add_markers(y = ~value, showlegend = FALSE) %>%
  layout(title = "Finales gewonnen", xaxis = list(title = ""), yaxis = list(title = ""))


plot_ly(data = alle_verliezers, x = ~Datum, y = ~value, color = ~variable, type = 'scatter', mode = 'lines') %>%
  add_markers(y = ~value, showlegend = FALSE)

plot_ly(data = iedereen, x = ~Datum, y = ~value, color = ~variable, type = 'scatter', mode = 'lines') %>%
  add_markers(y = ~value, showlegend = FALSE) # stuk

##### Deelnemers frequentietabel #####
# frequentietabel deelnemers maken
deelnemers <- as.data.frame(table(strsplit(paste(pingpong$Deelnemers, collapse = ", "), ", ")), stringsAsFactors = FALSE) 

#  dplyr::arrange(desc(Freq))
names(deelnemers) <- c("Naam", "Frequentie")
deelnemers <- dplyr::mutate(deelnemers, Proportioneel = round(Frequentie/nrow(pingpong) * 100, 1))

# frequentietabellen maken
winnaar_us <- as.data.frame(table(pingpong$Winnaar.US.Open), stringsAsFactors = FALSE)
winnaar_wim <- as.data.frame(table(pingpong$Winnaar.Wimbledon), stringsAsFactors = FALSE)
verliezer_us <- as.data.frame(table(pingpong$Verliezer.US.Open), stringsAsFactors = FALSE)
verliezer_wim <- as.data.frame(table(pingpong$Verliezer.Wimbledon), stringsAsFactors = FALSE)

# frequenties bij de juiste namen zetten
deelnemers2 <- left_join(deelnemers, winnaar_us, by = c("Naam" = "Var1"))
deelnemers2 <- left_join(deelnemers2, winnaar_wim, by = c("Naam" = "Var1"))
deelnemers2 <- left_join(deelnemers2, verliezer_us, by = c("Naam" = "Var1"))
deelnemers2 <- left_join(deelnemers2, verliezer_wim, by = c("Naam" = "Var1"))

# namen sorteren 
deelnemers$Naam <- factor(deelnemers$Naam, levels = unique(deelnemers$Naam)[order(deelnemers$Frequentie, dplyr::desc(deelnemers$Naam))])

# plot maken waarbij je een keuze kan maken tussen totaal en proportie
p <- plot_ly(data = deelnemers[deelnemers$Frequentie > 1,], y = ~Naam, hoverinfo = 'text', alpha = .5) %>%
  add_bars(x = ~Frequentie, showlegend = FALSE, text = ~paste0(Frequentie)) %>%
  add_bars(x = ~Proportioneel, visible = FALSE, showlegend = FALSE, text = ~paste0(round(Proportioneel), "%")) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    yaxis = list(title = ""),
    updatemenus = list( 
      list(y = 1.05, x = 0,
           buttons = list(
             list(method = "restyle",
                  args = list("visible", list(TRUE, FALSE)  ),
                  label = "Totaal"),
             
             list(method = "restyle",
                  args = list("visible", list(FALSE, TRUE)  ),
                  label = "Proportioneel")
           )
      )
    )
  )
p

##### Tabel met aantal finales gewonnen: keuze uit wimbledon, usopen of beide #####


# alle NAs omzetten naar 0
deelnemers2[is.na(deelnemers2)] <- 0
# deelnemers2$Naam <- factor(deelnemers2$Naam, levels = unique(deelnemers2$Naam))

# namen van variabelen aanpassen
names(deelnemers2) <- c("Naam", "Frequentie", "Proportioneel", "Winnaar_US", "Winnaar_WIM", "Verliezer_US", "Verliezer_WIM")

# extra variabelen toevoegen
deelnemers2$beide_gewonnen <- deelnemers2$Winnaar_US + deelnemers2$Winnaar_WIM
deelnemers2$beide_verloren <- deelnemers2$Verliezer_US + deelnemers2$Verliezer_WIM
deelnemers2$meegedaan_totaal <- deelnemers2$beide_gewonnen + deelnemers2$beide_verloren
deelnemers2$meegedaan_wim <- deelnemers2$Winnaar_WIM + deelnemers2$Verliezer_WIM
deelnemers2$meegedaan_us <- deelnemers2$Winnaar_US + deelnemers2$Verliezer_US

# hoe vaak win je elk toernooi?
deelnemers2$winratio_wim <- deelnemers2$Winnaar_WIM / deelnemers2$meegedaan_wim
deelnemers2$winratio_us <- deelnemers2$Winnaar_US / deelnemers2$meegedaan_us
deelnemers2$winratio_overall <- deelnemers2$beide_gewonnen / deelnemers2$meegedaan_totaal

# hoe vaak win je per x meedoen?

# visualiseren
p <- plot_ly(data = deelnemers2[deelnemers2$Frequentie > 1,], hoverinfo = 'text', alpha = .5,
             x = ~Naam) %>%
  add_bars(y = ~Winnaar_WIM, showlegend = FALSE, text = ~Winnaar_WIM) %>%
  add_bars(y = ~Winnaar_US, showlegend = FALSE, text = ~Winnaar_US, visible = FALSE) %>%
  add_bars(y = ~Verliezer_WIM, showlegend = FALSE, text = ~Verliezer_WIM, visible = FALSE) %>%
  add_bars(y = ~Verliezer_US, showlegend = FALSE, text = ~Verliezer_US, visible = FALSE) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    yaxis = list(title = ""),
    xaxis = list(title = ""),
    updatemenus = list(
      list(y = 1.05, x = 1,
           buttons = list(
             list(method = "restyle",
                  args = list("visible", list(TRUE, FALSE, FALSE, FALSE) ),
                  label = "Winnaar Wimbledon"),
             list(method = "restyle",
                  args = list("visible", list(FALSE, TRUE, FALSE, FALSE) ),
                  label = "Winnaar US Open"),
             list(method = "restyle",
                  args = list("visible", list(FALSE, FALSE, TRUE, FALSE) ),
                  label = "Verliezer Wimbledon"),
             list(method = "restyle",
                  args = list("visible", list(FALSE, FALSE, FALSE, TRUE) ),
                  label = "Verliezer US Open")
           )
      )
    )
  )

p

# mensen die nog niets hebben gewonnen komen nog steeds in de lijst voor
# sorteren lukt nog niet.
# misschien kan ik alles opdelen in meerdere datasets en bij restyle "data", list(df2) doen


p <- plot_ly(data = deelnemers[deelnemers$Frequentie > 1,], hoverinfo = 'text', alpha = .5) %>%
  add_bars(x = ~Winnaar_WIM, showlegend = FALSE, text = ~Winnaar_WIM, visible = FALSE,
           y = ~factor(Naam, levels = unique(Naam)[order(Winnaar_WIM, dplyr::desc(Naam))]) ) %>%
  add_bars(x = ~Winnaar_US, showlegend = FALSE, text = ~Winnaar_US, visible = TRUE,
           y = ~factor(Naam, levels = unique(Naam)[order(Winnaar_US, dplyr::desc(Naam))]) ) %>%
  add_bars(x = ~Verliezer_WIM, showlegend = FALSE, text = ~Verliezer_WIM, visible = FALSE,
           y = ~factor(Naam, levels = unique(Naam)[order(Verliezer_WIM, dplyr::desc(Naam))]) ) %>%
  add_bars(x = ~Verliezer_US, showlegend = FALSE, text = ~Verliezer_US, visible = FALSE,
           y = ~factor(Naam, levels = unique(Naam)[order(Verliezer_US, dplyr::desc(Naam))]) )


deelnemers$Naam <- factor(deelnemers$Naam, levels = unique(deelnemers$Naam)[order(deelnemers$Frequentie, dplyr::desc(deelnemers$Naam))])

deelnemers_wim <- table(c(pingpong$Winnaar.Wimbledon, pingpong$Verliezer.Wimbledon))
deelnemers_us <- table(c(pingpong$Winnaar.US.Open, pingpong$Verliezer.US.Open))

finalist <- table(c(pingpong$Winnaar.US.Open, pingpong$Verliezer.US.Open, pingpong$Winnaar.Wimbledon, pingpong$Verliezer.Wimbledon))


deelnemers
deelnemers_wim
deelnemers_us
finalist
winnaar_us
winnaar_wim


library(plotly)
library(reshape2)

# frequentietabel deelname
deelnemers$Naam <- factor(deelnemers$Naam, levels = unique(deelnemers$Naam)[order(deelnemers$Frequentie, dplyr::desc(deelnemers$Naam))])

plot_ly(data = deelnemers, x = ~Frequentie, y = ~Naam, type = 'bar', color = ~as.factor(Proportioneel),
        colors = c("red", "orange"), hoverinfo = 'text', 
        text = ~paste0('Naam: ', Naam, '<br>Aanwezig: ', Proportioneel, '%', '<br>Totaal: ',
                      Frequentie)) %>%
  config(displayModeBar = FALSE) %>%
  layout(showlegend = FALSE, yaxis = list(title = ""), title = "Aanwezigheid")

# tijdsgrafiek
pingmelt <- melt(pingpong, id.vars = c("Datum", "Dag", "Deelnemers", "Totaal"))
pingmelt$count <- ave(pingmelt$value==pingmelt$value, pingmelt$value, FUN = cumsum)


plot_ly(pingmelt,  x = ~Datum, y = ~value, type = 'scatter', color = ~value) 


# - deelnemers finale us open door de tijd heen
pingpong %>% select(1, 5:8) %>% melt(id.vars = "Datum") %>% 
  arrange(Datum) %>%
  mutate(count = ave(value == value, value, FUN = cumsum)) %>%
  
  
  plot_ly(x = ~Datum, y = ~count, color = ~value, type = 'scatter', mode = c('lines', 'marker')) %>%
  config(displayModeBar = FALSE) %>%
  layout(yaxis = list(title = "", tickmode = 'linear', tick0 = 0, dtick = 1), xaxis = list(title = ""),
         title = "Aantal keer deelgenomen aan een Grand Slam")


# wat komt erin?
# - deelname
# - aantal finales gewonnen waarbij je kan kiezen tussen wimbledon, usopen of beide + je kan kiezen tussen totaal en proportioneel
