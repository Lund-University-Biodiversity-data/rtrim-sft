# Du skall köra denna INNAN du kör Summary results. Har du redan kört Summary results får du köra om den. Det går också bra.

#Körning 240124: BEFIN gick igenom! För tallbit och röd glada måste de först köras separat och sedan får deras resultat läggas in manuellt 
# i de summerande tabellerna.Kör tallbiten enbart, precis som för alla arterna. Sedan koden nedan.
# RÅKA gick också igenom!

# VinPKT 
# Running BEFIN (Brambling, 248) in winter
# by changing the very highest values per route (50000+) 
# to  a lower number (now 50000)

#befin <- trimOutput$Art_248$data
#befin$count[befin$count>50000] <- 50000
#fix1 <- trim(count~site + time,
#             data = befin,
#             model = 2, overdisp = TRUE, serialcor = TRUE, autodelete = TRUE, changepoints = 'all', verbose = TRUE)
#
#trimOutput$Art_248$value <- fix1 # Adds the new output data to the overall list from the run of all species (if you did such)

# Running TABIT (Pine Grosbeak, 242) in winter
# by actively removing three changepoints (1984, 1986 and 1994). The years 1984 and 1994 would actually be removed anyway
# by autodelete (0 observations in these years), but 1986 has 1 bird seen, so 1986 must be removed in this way.

# 1. Kör först tallbit i appen som de andra arterna.
# 2. Kör koden nedan
# 3. Kör Summary results

load('trimOutput_tallbit19752022_Per3_totalvinter_pkt_2024-01-24_18_26_21.rdata') ##OBS!! Du måste ändra detta filnamn nedan också!"
trimOutput <- output
tabit <- trimOutput$Art_242$data
fix1 <- trim(count~site + time,
             data = tabit,
             model = 2, overdisp = TRUE, serialcor = TRUE, autodelete = TRUE, changepoints = (1:42)[-c(10, 12, 20)], verbose = TRUE)
trimOutput$Art_242$value <- fix1 # Adds the new output data to the overall list from the run of all species (if you did such)
output <- trimOutput
save(output, file = 'trimOutput_tallbit19752022_Per3_totalvinter_pkt_2024-01-24_18_26_21.rdata')

# VinPKT 

# !!Behövs inget extra för RÖGLA, bara kör utan Ser.Corr. i appen (precis som för råkan på SomPKT)!!

# Running RÖGLA (Red Kite, 043) in winter
# by changing the very highest values per route (50+) 
# to  a lower number (now 40, well 30)

rögla <- trimOutput$Art_43$data
# rögla$count[rögla$count>30] <- 30  Denna behövs inte, bara kör utan Ser.Corr. (precis som råkan på SomPKT)
fix1 <- trim(count~site + time,
             data = rögla,
             model = 2, overdisp = TRUE, serialcor = FALSE, autodelete = TRUE, changepoints = 'all', verbose = TRUE)

trimOutput$Art_43$value <- fix1 # Adds the new output data to the overall list from the run of all species (if you did such)



#**************************************************************************


# Run a single species with "manually" removed change points
mod1 <- trim(count~site + time,
          data = trimOutput$Art_248$data,
          model = 3, overdisp = FALSE, serialcor = FALSE)

tb <- trimOutput$Art_242$data
tb[tb$site=='530329-1_01'&tb$time==1986,'count'] <- 0
fix1 <- trim(count~site + time,
             data = tb,
             model = 2, overdisp = TRUE, serialcor = TRUE, autodelete = TRUE, changepoints = 'all', verbose = TRUE)

mod2 <- trim(count~site + time,
             data = glada,
             model = 2, overdisp = TRUE, serialcor = TRUE, autodelete = TRUE, changepoints = 'all', verbose = TRUE)

mod2 <- trim(count~site2 + time,
             data = befin,
             model = 3, overdisp = TRUE, serialcor = TRUE)

# This command is to load the rTrim manual
vignette('TRIM_methods_v2', package='rtrim')
