

#R Script - this is all the background Retrosheet scraping needed for project
## each section has its description on what its doing

########################################### Data Wrangling #####################################################

#########################################################################################
############## functions to pull from retrosheet, not needed anymore ####################
#########################################################################################

getRetrosheet <- function(type, year, team) {
  type <- match.arg(type, c("game", "play", "roster", "schedule"))
  
  if(type == "play" && missing(team)) {
    stop("argument 'team' must be supplied when 'type = \"play\"")
  }
  
  path <- switch(type,
                 "game" = "/gamelogs/gl%d.zip",
                 "play" = "/events/%deve.zip",
                 "roster" = "/events/%deve.zip",
                 "schedule" = "/schedule/%dSKED.ZIP")
  
  # Download to a temp location
  fullPath <- sprintf(paste0("https://www.retrosheet.org", path), year)
  if(!http_error(fullPath)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    RETRY("GET", url = fullPath, write_disk(tmp, overwrite=TRUE), timeout(15))
  } else {
    stop(sprintf("'%s' is not a valid url or path", fullPath))
  }
  fname <- unzip(tmp, list = TRUE)$Name
  
  
  if(type == "roster") {
    rosFiles <- grep(".ROS", fname, value = TRUE, fixed = TRUE)
    read <- lapply(rosFiles, function(x) {
      zcon <- unz(tmp, filename = x)
      o <- read.csv(zcon, header = FALSE, 
                    stringsAsFactors = FALSE,
                    col.names = c("retroID", "Last", "First", "Bat", "Throw", "Team", "Pos"))
      tibble(o)
    })
    out <- setNames(read, substr(rosFiles, 1L, 3L))
    return(out)
  }
  zcon <- unz(tmp, filename = paste0("TEAM", year))
  allTeams <- readLines(zcon)
  close(zcon)
  team <- match.arg(team, substr(allTeams, 1L, 3L))
  
  rgx <- paste(team, "EV", sep = ".")
  fnm <- grep(rgx, fname, value = TRUE, fixed = TRUE)
  zcon <- unz(tmp, filename = fnm)
  r <- readLines(zcon)
  close(zcon)
  g <- grepl("^id", r)
  sr <- unname(split(gsub("\"", "", r), cumsum(g)))
  sr
}

getTeamIDs <- function(year) {
  stopifnot(is.numeric(year), length(year) == 1L)
  path <- sprintf("https://www.retrosheet.org/events/%deve.zip", year)
  if (!http_error(path)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    RETRY("GET", url = path, write_disk(tmp, overwrite=TRUE), timeout(15))
  } else {
    available <- grep(year, getFileNames()$event)
    if(!length(available)) {
      return(NA)
      
    }
  }
  fname <- paste0("TEAM", year)
  unzip(tmp, files = fname)
  on.exit(unlink(fname), add = TRUE)
  
  read <- suppressWarnings(read.csv(fname, header = FALSE, stringsAsFactors = FALSE))[c(1, 4)]
  
  out <- structure(read[[1L]], .Names = read[[2L]])
  unname(out)
}



########################################### Data Wrangling1 #####################################################



##################################################################################################################
############## This section does what the above section did but better. Pulls all the roster data from each team##
############## by play by play, converts it into a tibble and then defines our pitchers and our hitters.##########
##################################################################################################################


write_pbp_year <- function(year) {
  print(year)
  team_names <- getTeamIDs(year)
  print(team_names)
  rosters <- getRetrosheet("roster", year)
  P = tibble()
  for (team in team_names) {
    print(c(team,year))
    pbp_team_year <- get_play_by_play(year, team, rosters)
    P <- bind_rows(P, pbp_team_year)
  }
  write_csv(P, str_glue("retro01_PA_{year}.csv"))
}

get_play_by_play <- function(year, team, rosters) {
  D <- getRetrosheet("play", year, team)
  result <- tibble()
  for (i in 1:length(D)) {
    E <- D[[i]]
    if (!all(str_detect(E, "^com"))) {
      # error in 1991, 92, 99 - D[[1]] is only `com`
      P <- pbpText_to_pbpTbl(E,rosters)
      result <- bind_rows(result, P)
    }
  }
  return(result)
}

#### transform retrosheet play-by-play text to play-by-play tibble
pbpText_to_pbpTbl <- function(E,rosters) {
  # get st_pitchers
  E <- E[!startsWith(E,"data")]
  E <- E[!startsWith(E,"version")]
  E <- c(E[1:7], E[20], E[26:length(E)])
  starters <- E[startsWith(E,"start")]
  st_pitchers <- starters[endsWith(starters,",1")]
  subbers <- E[startsWith(E,"sub")]
  for(i in 1:length(starters)) { starters[i] = substr(starters[i], 7,1000) }
  for(i in 1:length(st_pitchers)) { st_pitchers[i] = substr(st_pitchers[i], 7,1000) }
  for(i in 1:length(subbers)) { subbers[i] = substr(subbers[i], 5,1000) }
  players <- c(starters, subbers)
  E <- E[!startsWith(E,"start")]
  # get id
  id <- strsplit(E[startsWith(E,"id")], ",")[[1]][2]
  print(id)
  E <- E[!startsWith(E,"id")]
  # get info
  E1 <- E[startsWith(E,"info")]
  for(i in 1:length(E1)) { E1[i] = substr(E1[i], 6,1000) }
  E2 <- E1
  for(i in 1:length(E1)) { E1[i] = strsplit(E1[i], ",")[[1]][1] }
  for(i in 1:length(E2)) { E2[i] = strsplit(E2[i], ",")[[1]][2] }
  info.names = E1
  info.values = E2
  E <- E[!startsWith(E,"info")]
  # only keep pitcher `sub` and `play` in E (remove `com`,`badj`, and non-pitcher `sub`)
  E <- E[ startsWith(E,"play") | (startsWith(E,"sub") & endsWith(E,",1")) ]
  ###############################
  # add a column at the end of E: TRUE if pitcher-sub at this `play`, else FALSE
  for (i in 1:length(E)) {
    if (!startsWith(E[i], "sub")) {
      E[i] <- paste0(E[i], ",FALSE")
    }
  }
  x = which(startsWith(E, "sub"))-1
  for (i in x) {
    E[i] = paste0(str_sub(E[i],end=-7), ",TRUE")
  }
  ###############################
  subs.p = E[startsWith(E, "sub")]
  play = E[!startsWith(E, "sub")]
  for(i in 1:length(subs.p)) { subs.p[i] = substr(subs.p[i], 5,1000) }
  for(i in 1:length(subs.p)) { subs.p[i] = str_remove_all(subs.p[i], "\"") }
  for(i in 1:length(play)) { play[i] = substr(play[i], 6,1000) }
  ###############################
  
  ### create P tibble
  P = strsplit(play[1], ",")[[1]]
  for (i in 2:length(play)) {
    P = rbind(P, strsplit(play[i], ",")[[1]])
  }
  P = as.data.frame(P, row.names = FALSE, stringsAsFactors=FALSE)
  colnames(P) = c("inning","team","retroID","count","pitches","play","sub.here")
  P$inning = as.numeric(P$inning)
  P$team = as.numeric(P$team)
  P$retroID = as.character(P$retroID)
  P$count = as.character(P$count)
  P$pitches = as.character(P$pitches)
  P$play = as.character(P$play)
  P$sub.here = as.logical(P$sub.here)
  P = tibble(P)
  P <- P %>% mutate(game_id = id,
                    visteam = info.values[1],
                    hometeam = info.values[2],
                    site = info.values[3],
                    date = info.values[4]) #number = info.values[5], starttime = info.values[6], sky = info.values[7])
  
  ### create ST tibble (starting pitchers)
  ST = strsplit(st_pitchers[1], ",")[[1]]
  if (length(st_pitchers) >= 2) {
    for (i in 2:length(st_pitchers)) {
      ST = rbind(ST, strsplit(st_pitchers[i], ",")[[1]])
    }
  }
  ST = as.data.frame(ST, row.names = FALSE, stringsAsFactors=FALSE)
  colnames(ST) = c("pit.retroID", "pit.name", "pit.team", "V4", "V5")
  ST$pit.retroID = as.character(ST$pit.retroID)
  ST$pit.name = as.character(ST$pit.name)
  ST$pit.team = as.numeric(ST$pit.team) #- 1
  ST$V4 = NA 
  ST$V5 = NA 
  ST = tibble(ST) %>% select(!c(V4,V5))
  
  ### create SU tibble (subbed pitchers)
  if (!is.na(subs.p[1])) {
    SU = strsplit(subs.p[1], ",")[[1]]
    if (length(subs.p) >= 2) {
      for (i in 2:length(subs.p)) {
        SU = rbind(SU, strsplit(subs.p[i], ",")[[1]])
      }
      SU = as.data.frame(SU, row.names = FALSE, stringsAsFactors=FALSE)
    } else {
      SU = as.data.frame(t(SU), stringsAsFactors=FALSE)
    }
    colnames(SU) = c("pit.retroID", "pit.name", "pit.team", "V4", "V5")
    SU$pit.retroID = as.character(SU$pit.retroID)
    SU$pit.name = as.character(SU$pit.name)
    SU$pit.team = as.numeric(SU$pit.team) #- 1
    SU$V4 = NA 
    SU$V5 = NA 
    SU = tibble(SU) %>% select(!c(V4,V5))
  }
  
  ### Add starting pitchers to P
  P = P %>% mutate(pit.retroID = ifelse(team == 0, 
                                        (ST %>% filter(pit.team == 1))$pit.retroID, 
                                        (ST %>% filter(pit.team == 0))$pit.retroID),
                   pit.name = ifelse(team == 0, 
                                     (ST %>% filter(pit.team == 1))$pit.name, 
                                     (ST %>% filter(pit.team == 0))$pit.name))
  
  ### Add relieving pitchers to P
  if (!is.na(subs.p[1])) {
    s = which(P$sub.here)
    for (i in 1:nrow(SU)) {
      subbb = SU[i,]
      P = P %>% mutate(pit.retroID = ifelse(team != subbb$pit.team & row_number() >= s[i], subbb$pit.retroID, pit.retroID),
                       pit.name = ifelse(team != subbb$pit.team & row_number() >= s[i], subbb$pit.name, pit.name))
      
    }
  } 
  P = P %>% select(!c(sub.here))
  
  ##########################################################
  
  # add pit.hand column
  h = unique(P$hometeam)
  v = unique(P$visteam)
  H =  tibble(rosters[[h]]) %>% filter(Pos == "P") %>% select(!c(Bat,First,Last)) %>% 
    mutate(pit.hand=Throw, pit.retroID=retroID) %>% select(!c(Team,Pos,Throw,retroID)) 
  V =  tibble(rosters[[v]]) %>% filter(Pos == "P") %>% select(!c(Bat,First,Last)) %>% 
    mutate(pit.hand=Throw, pit.retroID=retroID) %>% select(!c(Team,Pos,Throw,retroID)) 
  HV = bind_rows(H,V)
  P <- left_join(P,HV,by="pit.retroID")
  
  
  ### add fieldPos, name columns
  PL = strsplit(players[1], ",")[[1]]
  if (length(players) >= 2) {
    for (i in 2:length(players)) {
      PL = rbind(PL, strsplit(players[i], ",")[[1]])
    }
  }
  PL = as.data.frame(PL, row.names = FALSE, stringsAsFactors=FALSE)
  colnames(PL) = c("retroID", "name", "team", "batPos", "fieldPos") 
  PL$retroID = as.character(PL$retroID)
  PL$name = as.character(PL$name)
  PL$team = as.numeric(PL$team)
  PL$batPos = as.numeric(PL$batPos)
  PL$fieldPos = as.numeric(PL$fieldPos)
  PL = tibble(PL) %>% select(!c(batPos,team))
  PL = PL %>% distinct(retroID, .keep_all = TRUE)
  P <- left_join(P,PL,by="retroID")
  
  # add BAT.HAND column
  H =  tibble(rosters[[h]]) %>% select(retroID, Bat)
  V =  tibble(rosters[[v]])  %>% select(retroID, Bat)
  HV = bind_rows(H,V) %>% rename(bat.hand = Bat)
  P <- left_join(P,HV,by="retroID")
  
  # YEAR
  P = P %>% mutate(year = substr(date,start=1,stop=4))
  
  # re-order the columns
  P = P %>% relocate(name, .after = retroID) %>% 
    relocate(bat.hand, .after = name) %>%
    relocate(fieldPos, .after = bat.hand) %>%
    relocate(pit.retroID, .after = fieldPos) %>%
    relocate(pit.name, .after = pit.retroID) %>%
    relocate(pit.hand, .after = pit.name) %>%
    relocate(year, .after = date) 
  
  return(P)
}

#build each teams roster data from 2010-2021
for(year in 2010:2021) {write_pbp_year(year)}





########################################### Data Wrangling Aggregate Games #####################################################

################################################################################################################################
############## This section cleans up the previous section's data by reorganizing the dates into one format ####################
################################################################################################################################


# Aggregate games from 2010-2021
IsDate <- function(date) {
  d1 = as.character(try(as.Date(date, "%Y-%m-%d"), silent = TRUE))
  d2 = as.character(try(as.Date(date, "%Y/%m/%d"), silent = TRUE))
  e1 = !is.na(d1)
  e2 = !is.na(d2)
  return(e1|e2)
}
date <- c("2012-04-08", "2020-12-31", "foo", "2021-31-12", "2015/04/06")
IsDate(date)

################ 

D <- tibble()
for (year in 2010:2021) {
  print(year)
  curr <- read_csv(str_glue("retro01_PA_{year}.csv")) 
  D <- bind_rows(D, curr)
}


write_csv(D, "retro02_PA_2010-2021.csv")




########################################### Data Wrangling2 #####################################################

########################################################################################################
############## This section creates all the new columns we need to run our analysis ####################
########################################################################################################


D = read_csv("retro02_PA_2010-2021.csv") 
output_filename = "retro03_PA_2010-2021.csv" 

################################

### initial columns: name changes
D1 = D %>% rename(GAME_ID = game_id,
                  PARK = site, 
                  DATE = date,
                  YEAR = year,
                  HOME_TEAM_ID = hometeam,
                  AWAY_TEAM_ID = visteam,
                  INNING = inning,
                  BAT_HOME_IND = team,
                  BAT_ID = retroID,
                  BAT_NAME = name,
                  BAT_HAND = bat.hand,
                  FIELD_POS = fieldPos,
                  PIT_ID = pit.retroID,
                  PIT_NAME = pit.name,
                  PIT_HAND = pit.hand,
                  COUNT = count,
                  PITCH_SEQ_TX = pitches, 
                  EVENT_TX = play) %>% 
  relocate(GAME_ID, PARK, DATE, YEAR, HOME_TEAM_ID, AWAY_TEAM_ID, INNING, BAT_HOME_IND, BAT_ID,
           BAT_NAME, BAT_HAND, FIELD_POS, PIT_ID, PIT_NAME, PIT_HAND, COUNT, PITCH_SEQ_TX, EVENT_TX)
print("D1")

### remove some "NP" (no-play) rows
D2 <- D1 %>% filter(EVENT_TX != "NP" | (!is.na(PITCH_SEQ_TX) & str_detect(PITCH_SEQ_TX, "[^\\.]")) )
print("D2")

# HIT_VAL === hit or not, and type of hit, c(0,1,2,3,4)
D3 = D2 %>% mutate(HIT_VAL =  ifelse(str_detect(EVENT_TX, "^S") & !str_detect(EVENT_TX, "^SB") & 
                                       !str_detect(EVENT_TX, "^SF") & !str_detect(EVENT_TX, "^SH"), 1,
                                     ifelse(str_detect(EVENT_TX, "^D") & !str_detect(EVENT_TX, "^DI"), 2,
                                            ifelse(str_detect(EVENT_TX, "^T"), 3,
                                                   ifelse(str_detect(EVENT_TX, "^H") & !str_detect(EVENT_TX, "^HP"), 4, 0)))))
print("D3")

# HIT_BINARY === 1 if hit else 0
D4 = D3 %>% mutate(HIT_BINARY = HIT_VAL > 0)
print("D4")

# BATTER_SEQ_NUM, ORDER_CT
# For BATTER_SEQ_NUM, do not count the same player twice in a row; hence use `consecutive.bat.row`
D5 <- D4 %>%  group_by(GAME_ID, BAT_HOME_IND) %>%
  mutate(consecutive.bat.row = lag(BAT_ID)==BAT_ID,
         consecutive.bat.row = ifelse(is.na(consecutive.bat.row), FALSE, consecutive.bat.row),
         BATTER_SEQ_NUM = cumsum(!consecutive.bat.row), #row_number(),
         ORDER_CT = 1 + (BATTER_SEQ_NUM-1) %/% 9) %>%
  ungroup() 
print("D5")

# HAND_MATCH 
# check unique(D$BAT_HAND) and unique(D$PIT_HAND)
D6 <- D5 %>% mutate(HAND_MATCH = ifelse(is.na(BAT_HAND) | is.na(PIT_HAND), NA,
                                        ifelse(BAT_HAND == "B" | PIT_HAND == "B", TRUE,
                                               BAT_HAND == PIT_HAND)))
print("D6")

# SP_IND, PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
D7 <- D6 %>%  group_by(GAME_ID, BAT_HOME_IND) %>% mutate(first.p = first(PIT_ID)) %>% ungroup() %>%
  group_by(GAME_ID, PIT_ID) %>%
  mutate(SP_IND = (PIT_ID == first.p)) %>%
  ungroup() %>% select(!c(first.p))
print("D7")

result = D7
write_csv(result, output_filename)




########################################### Data Wrangling3 #####################################################


#########################################################################################
############## This section defines the leagues separately and then combines them.#######
############## It pulls from a different dataset, but adds it to the main one. ##########
#########################################################################################


input_filename = "retro03_PA_2010-2021.csv"
output_filename = "retro04_PA_2010-2021.csv"
D <- read_csv(input_filename)

Div = read_csv("~/R-Master-Folder/mlb_divisions_dataset.csv")
DivT = as_tibble(cbind(team = names(Div), t(Div)), .name_repair = 'unique')

################################

{
  ### make sure no NA years
  D <- D %>% mutate(YEAR = ifelse(GAME_ID == "SEA201105160", 2011, YEAR),
                    DATE = ifelse(GAME_ID == "SEA201105160", as.Date("2011-05-16"), DATE))
  print(c("if this is 0 then we good:", nrow(D %>% filter(is.na(YEAR)))))
  
  # HOME_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
  # AWAY_DIV {NL_E, NL_C, NL_W, AL_E, AL_C, AL_W}
  # HOME_LEAGUE {AL, NL}
  # AWAY_LEAGUE {AL, NL}
  # TEAM_MATCH {IN_DIV, IN_LEAGUE}
  compute_divs <- function(yr, TEAM_IDS) { 
    print(yr)
    f <- function(team_id) {
      Div[yr >= Div$start & yr <= Div$end, ][[team_id]]
    }
    x = sapply(TEAM_IDS, f)
    unname(x)
  }
  D1 = D %>% group_by(YEAR) %>%
    mutate(HOME_DIV = compute_divs(unique(YEAR), HOME_TEAM_ID),
           AWAY_DIV = compute_divs(unique(YEAR), AWAY_TEAM_ID),
           HOME_LEAGUE = str_sub(HOME_DIV,1,2),
           AWAY_LEAGUE = str_sub(AWAY_DIV,1,2),
           IN_DIV = (HOME_DIV == AWAY_DIV),
           IN_LEAGUE = (HOME_LEAGUE == AWAY_LEAGUE)) %>%
    ungroup()
  print("D1")
  
  ###########
  
  result = D1
  write_csv(result, output_filename)
}




########################################### Data Wrangling4 #####################################################


######################################################################################################################
############## Defines events as they happen in a game. Sets outs recorded per event and total outs in game ##########
######################################################################################################################


input_filename = "retro04_PA_2010-2021.csv"
output_filename = "retro05_PA_2010-2021.csv"
D <- read_csv(input_filename)
D00 <- D %>% filter(YEAR %in% 2010:2021)

################################

compute_A <- function(a) {
  x1 = str_detect(a, "^C/E") | str_detect(a, "^S") | str_detect(a, "^D") | str_detect(a, "^T") | str_detect(a, "^E") | 
    str_detect(a, "^FC") | # ignore fielder's choice. looks at the baserunner activity
    str_detect(a, "^FLE") | str_detect(a, "^HP") | str_detect(a, "^HR") | str_detect(a, "^H") |
    str_detect(a, "^NP") | str_detect(a, "^W\\+SB") | str_detect(a, "^IW\\\\+SB") | str_detect(a, "^I\\+SB") |
    str_detect(a, "^W\\+PB") | str_detect(a, "^IW\\+PB") | str_detect(a, "^I\\+PB") | 
    str_detect(a, "^W\\+WP") | str_detect(a, "^IW\\+WP") | str_detect(a, "^I\\+WP") | 
    str_detect(a, "^W\\+E") | str_detect(a, "^IW\\+E") | str_detect(a, "^I\\+E") | 
    str_detect(a, "^W\\+OA") | str_detect(a, "^BK") | str_detect(a, "DI") | str_detect(a, "^OA") |
    str_detect(a, "^PB") | str_detect(a, "^WP") | str_detect(a, "^SB") |
    str_detect(a, "^K\\+E") | str_detect(a, "^K[0-9]*\\+OA")  # sometimes 0, sometimes 1 --> look at baserunner advances
  # return(0)
  
  x2 = (str_detect(a, "^W") & !str_detect(a, "^W\\+")) |
    (str_detect(a, "^IW") & !str_detect(a, "^IW\\+")) | 
    (str_detect(a, "^I") & !str_detect(a, "^I\\+")) 
  # return(0)
  
  x3 = str_detect(a, "^W\\+CS") | str_detect(a, "^IW\\+CS") | str_detect(a, "^I\\+CS") | 
    str_detect(a, "^W\\+PO") | str_detect(a, "^IW\\+PO") | str_detect(a, "^I\\+PO")
  # return(1)
  
  R = ifelse(x1 | x2, 0,
             ifelse(x3, 1, 
                    NA 
             ))
  
  return(R)
}
compute_K <- function(a,b) {
  BBB = str_detect(b, "B-") | str_detect(b, "BX") 
  x4 = str_detect(a, "^K[0-9]*\\+PB") | str_detect(a, "^K[0-9]*\\+WP") | str_detect(a, "^K[0-9]*\\+SB") |
    str_detect(a, "^K\\/FO") 
  xxx.b = str_count(b, "BX") - str_count(b, "BX[123H]*\\([0-9]*E") #str_count(b, "B-") - str_count(b, "B-[123H]*\\([0-9]*E") +
  xxx.nob = str_count(b, "[123H]X[123BH]") - str_count(b, "[123H]X[123BH]\\([0-9]*E")
  
  x5 = str_detect(a, "^K[0-9]*\\+CS") | str_detect(a, "^K[0-9]*\\+PO")
  x6 = str_detect(a, "^K") & !x4 & !x5
  x4 = x4 | x6
  
  AAA = str_detect(a, "\\([0-9]*E")
  
  R = ifelse(x4 & BBB, 0 + xxx.b + xxx.nob,
             ifelse(x4 & !BBB, 1 + xxx.nob, 
                    ifelse(x5 & AAA, 1 + xxx.nob,
                           ifelse(x5 & !AAA, 2, 
                                  NA))))
  
  return(R)
  
  # # return(0)
  # 
  # x6 = str_detect(a, "^K[0-9]*\\+CS") | str_detect(a, "^K[0-9]*\\+PO")
  # # return(2)
  # 
  # x7 = str_detect(a, "^K") & !x4 & !x5 & !x6
  # # return(1)
  # 
  # x8 = !x4 & !x5 & !x6 & !x7
}

################################

{
  # Duplicate row problem
  D0 = D00 %>% distinct(across(c(GAME_ID, INNING, BAT_HOME_IND, BAT_ID, PIT_ID, PITCH_SEQ_TX, EVENT_TX, BATTER_SEQ_NUM)), .keep_all = TRUE)
  
  
  # EVENT_OUTS_CT === number of outs recorded at that event
  A0 = D0 %>% mutate(
    ################################
    a = str_split_fixed(EVENT_TX, "\\.", 2)[,1], # string, description of the basic play
    b = str_split_fixed(EVENT_TX, "\\.", 2)[,2], # string, the advancement of any runners
    a = str_remove_all(a, "\\!"),
    starts.with.num = str_count(a, "^[0-9]"),
    num.outs.startsWithNum = str_count(a, "^[0-9]*") - str_count(a, "^[0-9]*E"), # 63/G --> 1 out, 6E3/G --> 0 outs
    num.outs.cs = str_count(a, "CS[23H]") - str_count(a, "CS[23H]\\([0-9]*E"), # CS2(E1/TH).3-H(NR);1-3 --> 0 outs
    num.outs.po = ifelse(str_detect(a, "^POCS"), 
                         str_count(a, "POCS[H123]") - str_count(a, "POCS[H123]\\([0-9]*E"),
                         str_count(a, "PO[H123]") - str_count(a, "PO[H123]\\([0-9]*E")),
    A = ifelse(starts.with.num, num.outs.startsWithNum,
               ifelse(str_detect(a, "CS") & !str_detect(a, "^K+CS"), num.outs.cs, # str_detect(a, "^CS")
                      ifelse(str_detect(a, "PO") & !str_detect(a, "^K+PO"), num.outs.po, # includes "POCS" # str_detect(a, "^PO")
                             compute_A(a)  
                      ))),
    ################################
    b = str_remove_all(b, "\\(NR\\)"),
    b = str_remove_all(b, "\\(UR\\)"),
    b = str_remove_all(b, "THH"),
    b = str_remove_all(b, "TH"),
    b = str_remove_all(b, "\\/"),
    b = str_remove_all(b, "\\(\\)"),
    num.baserunner.outs = str_count(b, "[123BH]X[123BH]") - 
      str_count(b, "[123BH]X[123BH]\\([0-9]*E") +
      str_count(b, "[123BH]X[123BH]\\([0-9]*E[0-9]*\\)\\([0-9]*\\)"),
    B = num.baserunner.outs,
    ################################
    dp = str_detect(EVENT_TX, "DP"),
    tp = str_detect(EVENT_TX, "TP"),
    K = compute_K(a,b),
    EVENT_OUTS_CT = ifelse(dp, 2, ifelse(tp, 3, ifelse(is.na(K), A+B, K)))
  ) %>% select( c(names(D0),EVENT_OUTS_CT) )
  D1 = left_join(D0, A0)
  print("D1")  
  
  ################################
  
  # OUTS_CT === number of outs PRIOR TO the play
  A1 = D1 %>% group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
    mutate(OUTS_CT_after = cumsum(EVENT_OUTS_CT)) %>%
    ungroup() %>%
    mutate(OUTS_CT = OUTS_CT_after - EVENT_OUTS_CT) %>%
    select(!c(OUTS_CT_after)) 
  D2 = left_join(D1, A1)
  print("D2")
  
  ################################
  
  result = D2
  write_csv(result, output_filename)
}

########################################################
########### Checks for EVENT_OUTS_CT ###################
########################################################

{
  # outs check: make sue all OUTS_CT are in c(0,1,2)
  # outs check: make sure the inning ends with 3 outs (and drop the last inning)
  # this tibble should be empty
  t1 = result %>% filter(OUTS_CT >= 3) %>%
    select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT)
  t2 = result %>% group_by(GAME_ID) %>% filter(INNING < max(INNING)) %>% ungroup() %>%
    group_by(GAME_ID, BAT_HOME_IND, INNING) %>% slice_tail() %>% 
    filter(replace_na(EVENT_OUTS_CT + OUTS_CT != 3, TRUE)) %>%  ungroup() %>%
    select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT)
  t3 = bind_rows(t1,t2) %>% distinct()
  View(t3)
}

## Pick random games to check

## specific game and inning check
game = "NYN201205060" 
inning = 6
View(result %>% filter(GAME_ID == game, INNING == inning) %>%
       select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))

## specific full game check
game = "WAS202009270"
View(result %>% filter(GAME_ID == game) %>%
       select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))




########################################### Data Wrangling5 #####################################################


#################################################################################################################
############## wOBA scraping then defining wOBA as it happens in a game. Getting batter and pitcher quality.#####
############## Preliminary work on creating moving averages for pitchers and batters wOBA. ######################
#################################################################################################################


input_filename = "retro05_PA_2010-2021.csv"
output_filename = "retro06_PA_2010-2021.csv"
E <- read_csv(input_filename)
E0 <- E

################################

### scrape WOBA WEIGHTS
content <- read_html("https://www.fangraphs.com/guts.aspx?type=cn")
tables <- content %>% html_table(fill = TRUE)
W <- tables[[9]]

################################

{
  # EVENT_CODE === {IW, W, HP, NA}  --> [need for wOBA calculation]
  E1 = E0 %>% mutate(EVENT_CODE =  ifelse(str_detect(EVENT_TX, "IW"), "IW",
                                          ifelse(str_detect(EVENT_TX, "^W") & !str_detect(EVENT_TX, "^WP"), "W",
                                                 ifelse(str_detect(EVENT_TX, "HP"), "HP", "other" ))))
  print("E1")
  
  # PA_IND (plate appearance) (so, not a substitution or stolen base). 
  # Definition:   https://en.wikipedia.org/wiki/Plate_appearance
  E2 <- E1 %>%  group_by(GAME_ID, BAT_HOME_IND) %>%
    mutate(x = lead(BAT_ID) != BAT_ID,
           x = replace_na(x, TRUE),
           PA_IND_0 = x) %>% select(!c(x)) %>%
    ungroup() 
  # 1. A batter is not credited with a plate appearance if, while batting, a preceding runner is put out on the basepaths for the 
  #    third out in a way other than by the batter putting the ball into play (i.e., picked off, caught stealing).
  E2a <- E2 %>% group_by(GAME_ID, BAT_HOME_IND) %>% slice_tail() %>%
    mutate(x = as.logical(str_count(EVENT_TX, "CS[23H]") - str_count(EVENT_TX, "CS[23H]\\([0-9]*E"))) %>% ungroup()
  E2b <- left_join(E2, E2a) %>% mutate(x = replace_na(x, FALSE)) %>% mutate(PA_IND_1 = PA_IND_0 & !x) %>% select(!c(x))
  # 2. A batter is not credited with a plate appearance if, while batting, the game ends as the winning run scores from third base on a 
  #    balk, stolen base, wild pitch, or passed ball.
  E2c <- E2b %>% group_by(GAME_ID) %>% slice_tail() %>%
    mutate(x = as.logical(str_detect(EVENT_TX, "^BK") | str_detect(EVENT_TX, "^SB") | 
                            str_detect(EVENT_TX, "^WP") | str_detect(EVENT_TX, "^PB"))) %>%
    ungroup()
  E2d <- left_join(E2b, E2c) %>% mutate(x = replace_na(x, FALSE)) %>% mutate(PA_IND_2 = PA_IND_1 & !x) %>% select(!c(x))          
  # 3. A batter may or may not be credited with a plate appearance (and possibly at bat) in the rare instance when he is replaced by a 
  #    pinch hitter after having already started his turn at bat. Under Rule 9.15(b), the pinch hitter would receive the 
  #    plate appearance (and potential of an at-bat) unless the original batter  is replaced when having 2 strikes against him 
  #    and the pinch hitter subsequently completes the strikeout, in which case the plate appearance and at-bat are charged 
  #    to the first batter.   
  E2e <- E2d %>% mutate(PA_IND_3 = ifelse(EVENT_TX == "NP", FALSE, PA_IND_2))
  
  
  E2_final <- E2e %>% mutate(PA_IND = PA_IND_3) %>% select(names(E1), PA_IND)
  rm(E2); rm(E2a); rm(E2b); rm(E2c); rm(E2d); rm(E2e);
  print("E2")
  
  # AB_IND (at bat)
  # AT-BAT is a PLATE-APPEARANCE without {SF,SH(sacBunt),W,IW,HP,C(catcher interference)}
  # https://www.retrosheet.org/eventfile.htm
  E3 <- E2_final %>% mutate(AB_IND = PA_IND & !(str_detect(EVENT_TX, "SF") & !str_detect(EVENT_TX, "\\/FL")) &
                              !(str_detect(EVENT_TX, "SH") & !str_detect(EVENT_TX, "\\/FL")) &
                              #!(str_detect(EVENT_TX, "^W") & !str_detect(EVENT_TX, "^WP")) & 
                              !str_detect(EVENT_TX, "^W") &
                              !str_detect(EVENT_TX, "^IW") & 
                              !str_detect(EVENT_TX, "^HP") & !str_detect(EVENT_TX, "^C"))
  print("E3")
  
  # WOBA_APP === TRUE iff it is a wOBA-appearance, i.e. in {AB,W,SF,HP}\{IW}
  # SH (sacrifice hit/bunt) is not part of wOBA!
  E4 <- E3 %>% mutate(WOBA_APP = (AB_IND | (str_detect(EVENT_TX, "^W") & !str_detect(EVENT_TX, "^WP")) | 
                                    (str_detect(EVENT_TX, "SF") & !str_detect(EVENT_TX, "\\/FL")) | #str_detect(EVENT_TX, "SF") |
                                    str_detect(EVENT_TX, "^HP")) & 
                        !str_detect(EVENT_TX, "^IW"))
  # E4 <- E3 %>% mutate(WOBA_APP = PA_IND & !str_detect(EVENT_TX, "^IW") & !str_detect(EVENT_TX, "^C") &  !str_detect(EVENT_TX, "^INT") )  
  print("E4")
  
  # EVENT_WOBA === wOBA of this event
  # https://www.fangraphs.com/guts.aspx?type=cn
  # HP, is an AB and PA.
  # SH, SF, IW, W are PA but not AB
  # an event is a WOBA_EVENT iff it is an {AB, W, SH, SF, HP} but not {IW}. Equivalently, {PA}\{IW}
  # include all plate appearances as wOBA except intentional walks !!!
  E5 = E4 %>% group_by(YEAR) %>% mutate(EVENT_WOBA = 
                                          ifelse(WOBA_APP & HIT_VAL == 1, W[W$Season == unique(YEAR),]$w1B, # single
                                                 ifelse(WOBA_APP & HIT_VAL == 2, W[W$Season == unique(YEAR),]$w2B, # double
                                                        ifelse(WOBA_APP & HIT_VAL == 3, W[W$Season == unique(YEAR),]$w3B, # triple
                                                               ifelse(WOBA_APP & HIT_VAL == 4, W[W$Season == unique(YEAR),]$wHR, # HR
                                                                      ifelse(WOBA_APP & EVENT_CODE == "W", W[W$Season == unique(YEAR),]$wBB, # uBB / NIBB
                                                                             ifelse(WOBA_APP & EVENT_CODE == "HP", W[W$Season == unique(YEAR),]$wHBP, # HBP / HP
                                                                                    #ifelse(EVENT_CD==18, 0.92, # RBOE (reached base on error) --> no longer in the woba formula
                                                                                    #ifelse(PA_IND &  (EVENT_CODE != "IW"), 0, NA )))))))) %>% ungroup()
                                                                                    0))))))) %>% ungroup()
  print("E5")
  
  # WOBA_CUMU_BAT (INDIVIDUAL BATTER'S QUALITY)
  E6 <- E5 %>% group_by(YEAR, BAT_ID) %>%
    mutate(cumu.woba.sum.b = cumsum(replace_na(EVENT_WOBA, 0)),
           cumu.woba.denom.b = cumsum(replace_na(WOBA_APP, 0)),
           WOBA_CUMU_BAT = cumu.woba.sum.b/cumu.woba.denom.b) %>% 
    ungroup()
  print("E6")
  
  # WOBA_CUMU_PIT (INDIVIDUAL PITCHER'S QUALITY)
  E7 <- E6 %>% group_by(YEAR, PIT_ID) %>%
    mutate(cumu.woba.sum.p = cumsum(replace_na(EVENT_WOBA, 0)),
           cumu.woba.denom.p = cumsum(replace_na(WOBA_APP, 0)),
           WOBA_CUMU_PIT = cumu.woba.sum.p/cumu.woba.denom.p) %>% 
    ungroup()
  print("E7")
  
  # EVENT_RUNS === number of runs recorded during this event
  # EVENT_ER_CT === number of earned runs recorded during this event (take into accoount UR)
  # EVENT_RBI_CT === number of RBIs recorded during this event (take into accoount NR)
  E8 = E7 %>% mutate(run_tx = str_extract(EVENT_TX, "([^.]+$)"),
                     num.home = str_count(run_tx, "-H"),
                     no.rbi = str_count(run_tx, "NR"),
                     unearned.runs = str_count(run_tx, "UR"), 
                     hr.ind = ifelse(HIT_VAL == 4, 1, 0),
                     EVENT_RUNS = num.home + hr.ind,
                     EVENT_ER_CT = EVENT_RUNS - unearned.runs,
                     EVENT_RBI_CT = EVENT_RUNS - no.rbi) %>%
    select(!c(run_tx, num.home, no.rbi, unearned.runs, hr.ind))
  print("E8")
  
  
  # EVENT_PITCH_COUNT== pitch count per event
  # https://www.retrosheet.org/datause.txt    --> field 5
  # pitches: C,S,B,F,X,T,H,L,M,P,K,U,Q,R   --> 14
  # not a pitch: N,V,1,2,3,+,>,*,.,        --> 9
  E9 = E8 %>% mutate(p = str_remove_all(PITCH_SEQ_TX, "[NV123\\+\\>\\*\\.]"),
                     EVENT_PITCH_COUNT = str_length(p)) %>%
    select(!c(p))
  print("E9")
  
  # PITCH_COUNT_CUMU, PITCH_COUNT_FINAL
  E10 <- E9 %>% group_by(GAME_ID, PIT_ID) %>%
    mutate(PITCH_COUNT_CUMU = cumsum(replace_na(EVENT_PITCH_COUNT, 0)),
           PITCH_COUNT_FINAL = sum(EVENT_PITCH_COUNT, na.rm=TRUE)) %>%
    ungroup()
  print("E10")}

##############################

R = E10
R_ = R %>% select(!c(cumu.woba.sum.b, cumu.woba.denom.b, cumu.woba.sum.p, cumu.woba.denom.p))
write_csv(R_, output_filename)

##############################
########### CHECKS ###########
##############################

y = 2020 
w = W[W$Season == y,]
# CHECK WOBA_CUMU_BAT
{
  ### read TRUE WOBAS
  TW0 = read_csv(str_glue("true_woba_{y}.csv"))
  TW = TW0 %>% mutate(BAT_NAME = paste(first_name, last_name),wOBA_true=woba) %>% select(BAT_NAME, wOBA_true) %>% 
    arrange(-wOBA_true) %>% 
    mutate(BAT_NAME = str_replace_all(BAT_NAME, "á", "a"),
           BAT_NAME = str_replace_all(BAT_NAME, "é", "e"),
           BAT_NAME = str_replace_all(BAT_NAME, "í", "i"),
           BAT_NAME = str_replace_all(BAT_NAME, "ó", "o"),
           BAT_NAME = str_replace_all(BAT_NAME, "ñ", "n"))
  # https://baseballsavant.mlb.com/leaderboard/expected_statistics?type=batter&year=2020&position=&team=&min=100&sort=11&sortDir=desc
  # https://baseballsavant.mlb.com/leaderboard/expected_statistics?type=batter&year=2015&position=&team=&min=100&sort=11&sortDir=desc
  R1 = R %>% filter(YEAR == y) %>% group_by(BAT_ID) %>% filter(row_number() == n()) %>% filter(cumu.woba.denom.b >= 150) %>% 
    ungroup() %>% mutate(wOBA=round(WOBA_CUMU_BAT,3)) %>% 
    arrange(-WOBA_CUMU_BAT) %>% select(BAT_ID, BAT_NAME, YEAR, wOBA, cumu.woba.denom.b, WOBA_CUMU_BAT) %>%
    left_join(TW) %>% relocate(wOBA_true, .after = wOBA) %>% mutate(wOBA_diff = wOBA_true - wOBA) %>% relocate(wOBA_diff, .after = wOBA_true)
  View(R1)
  
  R1a = R1 %>% filter(wOBA_diff != 0)
  View(R1a)
  
# Check individual player wOBA
  # "Josh Reddick" in 2015 agrees, see
  # https://www.fangraphs.com/players/josh-reddick/3892/stats?position=OF
  N =  "Josh Reddick"
  R2 = R %>% filter(YEAR== y, BAT_NAME== N) %>% 
    summarise(G=length(unique(GAME_ID)), AB=sum(AB_IND), PA = sum(PA_IND), 
              H=sum(HIT_BINARY), S=sum(HIT_VAL==1), D=sum(HIT_VAL==2), T= sum(HIT_VAL==3), HR= sum(HIT_VAL==4),
              W= sum(EVENT_CODE=="W"), IW=sum(EVENT_CODE=="IW"), BB=W+IW, HP= sum(EVENT_CODE=="HP"), 
              AVG = H/AB, 
              wOBA_num = (S*w$w1B + D*w$w2B + T*w$w3B + HR*w$wHR + W*w$wBB + HP*w$wHBP), 
              wOBA_APP = last(cumu.woba.denom.b), 
              wOBA = wOBA_num/(wOBA_APP)) %>%
    select(!c(W)) %>% relocate(BB, .before=IW)
  R2
}

# CHECK ERA, PITCH COUNT from
# "EVENT_RUNS"          "EVENT_ER_CT"        "EVENT_RBI_CT"        "EVENT_PITCH_COUNT"   "PITCH_COUNT_CUMU"    "PITCH_COUNT_FINAL"  
{
  # https://www.mlb.com/stats/pitching/earned-runs/2020?playerPool=QUALIFIED&sortState=asc
  S1 = R %>% filter(YEAR == y) %>% group_by(PIT_ID) %>% mutate(num_ER = sum(EVENT_ER_CT), num_P = sum(EVENT_PITCH_COUNT)) %>% 
    filter(row_number() == n(), num_P >= 1050) %>% ungroup() %>% select(PIT_NAME, num_ER, num_P) %>% arrange(num_ER)
  View(S1)
}




########################################### Data Wrangling7 #####################################################


##############################################################################################
############## Create pitcher rest days and add it main data, max rest days of six. ##########
##############################################################################################


###########################################

input_filename = "retro06_PA_2010-2021.csv"
output_filename = "retro07_PA_2010-2021.csv"
E <- read_csv(input_filename)
E0 <- E 

# DATE
E1 <- E0 %>% mutate(DATE = paste(str_sub(GAME_ID,4,7), str_sub(GAME_ID,8,9), str_sub(GAME_ID,10,11), sep="-"))
print("E1")

# DAYS_SINCE_SZN_START 
E2 <- E1 %>% group_by(YEAR) %>% arrange(DATE) %>%
  mutate(FIRST_DATE = min(DATE, na.rm=TRUE),
         DAYS_SINCE_SZN_START = as.vector(difftime(DATE, FIRST_DATE, units="days"))) %>%
  ungroup() %>% select(!c(FIRST_DATE))
print("E2")

# PIT_REST
max_date_val = 6
E3 <- E2 %>% group_by(YEAR, PIT_ID) %>% arrange(DATE) %>%
  mutate(PIT_REST = as.vector(difftime(DATE, lag(DATE), units="days"))) %>%
  ungroup() %>% 
  group_by(YEAR, PIT_ID, GAME_ID) %>%
  mutate(PIT_REST = first(PIT_REST),
         PIT_REST = pmin(PIT_REST, max_date_val)) %>%
  ungroup() %>% group_by(YEAR, PIT_ID) %>%
  mutate(PIT_REST = ifelse(DATE == min(DATE, na.rm=TRUE), max_date_val,  PIT_REST)) %>%
  ungroup() %>% arrange(GAME_ID) 
print("E3")

##############################

R = E3
write_csv(R, output_filename)




########################################### Data Wrangling8 #####################################################

#####################################################################################################
############## Further wOBA scraping. This finishes the work in the previous two sections.###########
############## Take prior season wOBA to calculate a moving average for the pitchers and batters.####
#####################################################################################################


################################

input_filename = "retro07_PA_2010-2021.csv"
output_filename = "retro_final_PA_2010-2021b.csv"
E <- read_csv(input_filename)

################################

### scrape WOBA WEIGHTS
content <- read_html("https://www.fangraphs.com/guts.aspx?type=cn")
tables <- content %>% html_table(fill = TRUE)
W <- tables[[9]]
W <- W %>% filter(Season == 2021) 

################################

{
  # WOBA_AVG_BAT, WOBA_AVG_PIT
  E0 <- E %>% rename(WOBA_AVG_BAT = WOBA_CUMU_BAT,
                     WOBA_AVG_PIT = WOBA_CUMU_PIT)
  
  # EVENT_WOBA_21 === wOBA of this event, using 2021 wOBA weights
  # https://www.fangraphs.com/guts.aspx?type=cn
  # HP, is an AB and PA.
  # SH, SF, IW, W are PA but not AB
  # an event is a WOBA_EVENT iff it is an {AB, W, SH, SF, HP} but not {IW}. Equivalently, {PA}\{IW}
  # ---> include all plate appearances as wOBA except intentional walks !!!
  E1 = E0 %>% mutate(EVENT_WOBA_21 = 
                       ifelse(WOBA_APP & HIT_VAL == 1, W$w1B, # single
                              ifelse(WOBA_APP & HIT_VAL == 2, W$w2B, # double
                                     ifelse(WOBA_APP & HIT_VAL == 3, W$w3B, # triple
                                            ifelse(WOBA_APP & HIT_VAL == 4, W$wHR, # HR
                                                   ifelse(WOBA_APP & EVENT_CODE == "W", W$wBB, # uBB / NIBB
                                                          ifelse(WOBA_APP & EVENT_CODE == "HP", W$wHBP, # HBP / HP
                                                                 0)))))))
  print("E1")
  
  # WOBA_AVG_BAT_21
  # NUM_WOBA_APP_BAT
  E2 <- E1 %>% group_by(YEAR, BAT_ID) %>%
    mutate(cumu.woba.sum.b = cumsum(replace_na(EVENT_WOBA_21, 0)),
           NUM_WOBA_APP_BAT = cumsum(replace_na(WOBA_APP, 0)),
           WOBA_AVG_BAT_21 = cumu.woba.sum.b/NUM_WOBA_APP_BAT) %>% 
    ungroup()
  print("E2")
  
  # WOBA_AVG_PIT_21
  # NUM_WOBA_APP_PIT
  E3 <- E2 %>% group_by(YEAR, PIT_ID) %>%
    mutate(cumu.woba.sum.p = cumsum(replace_na(EVENT_WOBA_21, 0)),
           NUM_WOBA_APP_PIT = cumsum(replace_na(WOBA_APP, 0)),
           WOBA_AVG_PIT_21 = cumu.woba.sum.p/NUM_WOBA_APP_PIT) %>% 
    ungroup()
  print("E3")
  
  # WOBA_FINAL_BAT, WOBA_FINAL_BAT_21, NUM_WOBA_APP_FINAL_BAT
  G4 <- E3 %>% 
    group_by(YEAR, BAT_ID) %>% 
    filter(row_number() == n()) %>%
    select(YEAR, BAT_ID, WOBA_AVG_BAT, WOBA_AVG_BAT_21, NUM_WOBA_APP_BAT) %>%
    rename(WOBA_FINAL_BAT = WOBA_AVG_BAT, 
           WOBA_FINAL_BAT_21 = WOBA_AVG_BAT_21,
           NUM_WOBA_APP_FINAL_BAT = NUM_WOBA_APP_BAT) %>%
    ungroup() %>%
    arrange(YEAR, BAT_ID)
  E4 <- E3 %>% left_join(G4)
  View(E4 %>% select(YEAR, BAT_NAME, WOBA_AVG_BAT_21, WOBA_FINAL_BAT_21, NUM_WOBA_APP_BAT, NUM_WOBA_APP_FINAL_BAT))
  
  # WOBA_FINAL_PIT, WOBA_FINAL_PIT_21, NUM_WOBA_APP_FINAL_PIT
  G5 <- E4 %>% 
    group_by(YEAR, PIT_ID) %>% 
    filter(row_number() == n()) %>%
    select(YEAR, PIT_ID, WOBA_AVG_PIT, WOBA_AVG_PIT_21, NUM_WOBA_APP_PIT) %>%
    rename(WOBA_FINAL_PIT = WOBA_AVG_PIT, 
           WOBA_FINAL_PIT_21 = WOBA_AVG_PIT_21,
           NUM_WOBA_APP_FINAL_PIT = NUM_WOBA_APP_PIT) %>%
    ungroup() %>%
    arrange(YEAR, PIT_ID)
  E5 <- E4 %>% left_join(G5)
  View(E5 %>% select(YEAR, PIT_NAME, WOBA_AVG_PIT_21, WOBA_FINAL_PIT_21, NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_PIT))
  
}

##############################

R = E5
R_ = R %>% select(!c(cumu.woba.sum.b, cumu.woba.sum.p))
write_csv(R_, output_filename)

##############################
########### CHECKS ###########
##############################

y=2021
w = W
{
  # CHECK WOBA_AVG_BAT_19
  R1 = R %>% group_by(BAT_ID) %>% filter(row_number() == n()) %>% filter(NUM_WOBA_APP_BAT >= 150) %>%
    ungroup() %>% mutate(wOBA=round(WOBA_AVG_BAT_21,3)) %>% filter(year == 2021) %>%
    arrange(-WOBA_AVG_BAT_21) %>% select(BAT_ID, BAT_NAME, YEAR, wOBA, NUM_WOBA_APP_BAT, WOBA_AVG_BAT_21) 
  View(R1)
}





########################################### Data Wrangling9 #####################################################



#########################################################################################################################
############## Recreation of baseball games. Start with a base state and then calculate all possible scenarios ##########
#########################################################################################################################


input_filename = "retro_final_PA_2010_2021b.csv"
output_filename = "retro_base_states_2010_2021c.csv"
E <- read_csv(input_filename)
E1 <- E 
E1 <- E1 %>% select(GAME_ID,INNING,BAT_HOME_IND,BAT_ID,WOBA_APP,EVENT_TX,)
rm(E)

# initialize BASE_STATE
E1$MAN_ON_1ST = 0
E1$MAN_ON_2ND = 0
E1$MAN_ON_3RD = 0

counter = 0
perform_base_progressions <- function(EE) {
  for (i in 1:nrow(EE)) {
    print(counter)
    assign("a", "new", envir = .GlobalEnv)
    counter <<- counter+1
    e_curr = EE[i,]
    event_tx = e_curr$EVENT_TX
    
    if (i == nrow(EE)) {
      # the last event of the inning -> no baserunners after that event
      EE[i,]$MAN_ON_1ST = NA
      EE[i,]$MAN_ON_2ND = NA
      EE[i,]$MAN_ON_3RD = NA
      return(EE)
    } else if (i > 1) {
      # account for men on base from the previous event
      e_prev = EE[i-1,]
      mo1 = e_prev$MAN_ON_1ST
      mo2 = e_prev$MAN_ON_2ND 
      mo3 = e_prev$MAN_ON_3RD;
    } else {
      # initialize men on base for the first event of the half inning
      mo1 = 0; mo2 = 0; mo3 = 0;
    }
    
    # move the baserunners
    mtb = move_the_baserunners(event_tx,mo1,mo2,mo3)
    mo1 = mtb[1]
    mo2 = mtb[2]
    mo3 = mtb[3]
    # account for the main result of this event (e.g.,single,walk,...)
    if (e_curr$WOBA_APP) {
      gmbe = get_main_base_event(event_tx)
      mo1 = ifelse(gmbe==1, 1, mo1)
      mo2 = ifelse(gmbe==2, 1, mo2)
      mo3 = ifelse(gmbe==3, 1, mo3)
    }
    # store result
    EE[i,]$MAN_ON_1ST = mo1
    EE[i,]$MAN_ON_2ND = mo2
    EE[i,]$MAN_ON_3RD = mo3
  }
  EE
}

move_the_baserunners <- function(event_tx,mo1,mo2,mo3) {
  b00 = str_split_fixed(event_tx, "\\.", 2)[,2]
  
  # remove both the 2X3 and the error. 
  b001 = str_remove_all(b00, "[B123]X[123H]\\(.?E.?\\)\\;?")
  b002 = ifelse(str_sub(b001,-1) == ";", str_sub(b001,1,-2), b001)
  
  bb1 = str_extract_all(b00, "[B123]X[123H]\\(.?E.?\\)")[[1]]
  bb2 = str_remove_all(bb1, "(\\(.+?\\))+")
  bb3 = str_replace_all(bb2,"X","\\-")
  bb4 = paste(bb3,collapse=";")
  b0 = ifelse(bb4 == "", b002,
              ifelse(b002 == "", bb4,
                     paste(c(b002,bb4),collapse=";")))
  
  # now remove all errors
  b = str_remove_all(b0, "(\\(.+?\\))+")
  
  a = str_split_fixed(event_tx, "\\.", 2)[,1]
  # stolen base SB
  if (str_detect(a, "SB2") & !str_detect(b, "1\\-") & !str_detect(b, "1X")) {
    b = paste0(b,"1-2")
  } else if (str_detect(a, "SB3") & !str_detect(b, "2\\-") & !str_detect(b, "2X")) {
    b = paste0(b,"2-3")
  } else if (str_detect(a, "SBH") & !str_detect(b, "3\\-") & !str_detect(b, "3X")) {
    b = paste0(b,"3-H")
  }
  # caught stealing CS & POCS
  a = str_replace(a,"POCS","CS")
  if (str_detect(a, "CS2") & !str_detect(b, "1\\-") & !str_detect(b, "1X")) {
    b = paste0(b,"1X2")
  } else if (str_detect(a, "CS3") & !str_detect(b, "2\\-") & !str_detect(b, "2X")) {
    b = paste0(b,"2X3")
  } else if (str_detect(a, "CSH") & !str_detect(b, "3\\-") & !str_detect(b, "3X")) {
    b = paste0(b,"3XH")
  }
  # picked off PO 
  a = str_remove(a, "PO[123]\\(.?E.?\\)")
  #(account for errors)
  if (str_detect(a, "PO1") & !str_detect(b, "1\\-") & !str_detect(b, "1X")) {
    b = paste0(b,"1X1")
  } else if (str_detect(a, "PO2") & !str_detect(b, "2\\-") & !str_detect(b, "2X")) {
    b = paste0(b,"2X2")
  } else if (str_detect(a, "PO3") & !str_detect(b, "3\\-") & !str_detect(b, "3X")) {
    b = paste0(b,"3X3")
  } 
  
  
  b1 = str_split_fixed(b, "\\;", 3)[,1]
  b2 = str_split_fixed(b, "\\;", 3)[,2]
  b3 = str_split_fixed(b, "\\;", 3)[,3]
  
  # baserunner who do not get thrown out
  c1 = str_split_fixed(b1, "\\-", 2)
  c2 = str_split_fixed(b2, "\\-", 2)
  c3 = str_split_fixed(b3, "\\-", 2)
  
  # baserunner who are attempted to get thrown out
  d1 = str_split_fixed(b1, "X", 2)
  d2 = str_split_fixed(b2, "X", 2)
  d3 = str_split_fixed(b3, "X", 2)
  
  # remove 1st baserunner from starting base
  mo1 = ifelse(c1[,1] == "1" | d1[,1] == "1", 0, mo1)
  mo2 = ifelse(c1[,1] == "2" | d1[,1] == "2", 0, mo2)
  mo3 = ifelse(c1[,1] == "3" | d1[,1] == "3", 0, mo3)
  # add 1st baserunner to destination base
  mo1 = ifelse(c1[,2] == "1", 1, mo1)
  mo2 = ifelse(c1[,2] == "2", 1, mo2)
  mo3 = ifelse(c1[,2] == "3", 1, mo3)
  # remove 2nd baserunner from starting base
  mo1 = ifelse(c2[,1] == "1" | d2[,1] == "1", 0, mo1)
  mo2 = ifelse(c2[,1] == "2" | d2[,1] == "2", 0, mo2)
  mo3 = ifelse(c2[,1] == "3" | d2[,1] == "3", 0, mo3)
  # add 2nd baserunner to destination base
  mo1 = ifelse(c2[,2] == "1", 1, mo1)
  mo2 = ifelse(c2[,2] == "2", 1, mo2)
  mo3 = ifelse(c2[,2] == "3", 1, mo3)
  # remove 3rd baserunner from starting base
  mo1 = ifelse(c3[,1] == "1" | d3[,1] == "1", 0, mo1)
  mo2 = ifelse(c3[,1] == "2" | d3[,1] == "2", 0, mo2)
  mo3 = ifelse(c3[,1] == "3" | d3[,1] == "3", 0, mo3)
  # add 3rd baserunner to destination base
  mo1 = ifelse(c3[,2] == "1", 1, mo1)
  mo2 = ifelse(c3[,2] == "2", 1, mo2)
  mo3 = ifelse(c3[,2] == "3", 1, mo3)
  
  c(mo1,mo2,mo3)
}

get_main_base_event <- function(event_tx) {
  a = str_split_fixed(event_tx, "\\.", 2)[,1]
  b = str_split_fixed(event_tx, "\\.", 2)[,2]
  if (str_detect(b, "B\\-") | str_detect(b, "BX")) {
    return(0)
  }
  
  mo1 = (str_detect(a, "^S") & !str_detect(a, "^SB")) | 
    str_detect(a, "^W") | str_detect(a, "^IW") |
    str_detect(a, "^WP") | str_detect(a, "^PB") | str_detect(a, "^HP") |
    str_detect(a, "^E") | str_detect(a, "^FC") 
  
  mo2 = str_detect(a, "^D") & !str_detect(a, "^DI")
  
  mo3 = str_detect(a, "^T")
  
  ifelse(mo1, 1, ifelse(mo2, 2, ifelse(mo3, 3, 0)))
}

E2 <- E1 %>% group_by(GAME_ID,INNING,BAT_HOME_IND) %>% 
  group_modify(~ perform_base_progressions(.x)) %>%
  mutate(MAN_ON_1ST = lag(MAN_ON_1ST, default=0),
         MAN_ON_2ND = lag(MAN_ON_2ND, default=0),
         MAN_ON_3RD = lag(MAN_ON_3RD, default=0)) %>%
  ungroup() %>% mutate(
    BASE_STATE = paste0(as.character(MAN_ON_1ST),as.character(MAN_ON_2ND),as.character(MAN_ON_3RD))
  )

##############################

R = E2
write_csv(R, output_filename)


##############################################################
########## Merge BASE_STATE with the entire dataset ##########
##############################################################

input_filename0 = "retro_base_states_2021_2021c.csv"
input_filename = "retro_final_PA_2010_2021b.csv"
output_filename = "retro_final_PA_2010_2021c.csv"
B <- R 
C <- read_csv(input_filename)

C$BASE_STATE = B$BASE_STATE
C = C %>% select(-c(EVENT_ER_CT, EVENT_RBI_CT)) 

write_csv(C, output_filename)



################################################# Data Wrangling10 ########################################


##########################################################################################
############## Reorganization of the main dataset, clean it up for easier use ############
##########################################################################################

input_filename = "retro_final_PA_2010_2021c.csv"
output_filename = "retro_final_PA_2010_2021d.csv"
E <- read_csv(input_filename)

# row_idx
E1 = E %>% mutate(row_idx = row_number()) %>% relocate(row_idx, .before=GAME_ID)
# remove the columns we are replacing
E2 = E1 %>% select(-c("WOBA_AVG_PIT","WOBA_AVG_BAT","WOBA_FINAL_BAT","WOBA_FINAL_PIT",
                      "WOBA_AVG_PIT_21","WOBA_AVG_BAT_21","WOBA_FINAL_BAT_21","WOBA_FINAL_PIT_21",
                      "NUM_WOBA_APP_BAT","NUM_WOBA_APP_PIT","NUM_WOBA_APP_FINAL_BAT","NUM_WOBA_APP_FINAL_PIT")) %>%
  relocate(EVENT_WOBA, .before = EVENT_WOBA_21)

##############################

R = E2
write_csv(R, output_filename)
