library(ggmap)
library(zipcode)
library(ggplot2)
library(gridExtra)
library(igraph)


#org_data = data.frame(tapply(org_airpo$AIRPORT,list(org_airpo$AIRPORT,org_airpo$STATE),length))

#barplot(org_data$tapply.org_airpo.AIRPORT..list.org_airpo.AIRPORT...length., names.arg = names(org_data))


#summary(flight.data$ARRIVAL_DELAY)


airlines = file.choose()
data.flights = read.csv(airlines, stringsAsFactors = F)
dim(data.flights)
flight = file.choose()
long = read.csv(flight, stringsAsFactors = F)
airports = data.frame(long)


#length(which(airports$DEPARTURE_TIME == '2400'))


fly = file.choose()
flight_name = read.csv(fly, stringsAsFactors = F)
airlines = data.frame(flight_name)
colnames(airlines) <- c('AIRLINE','AIRLINE_NAME')


###################################################################################################
# Cleaning
##################################################################################################

data.flights$ORIGIN_AIRPORT <- gsub("\\d", "", data.flights$ORIGIN_AIRPORT)
data.flights$DESTINATION_AIRPORT <- gsub("\\d", "", data.flights$DESTINATION_AIRPORT)
data.flights <- data.flights[data.flights$ORIGIN_AIRPORT != "",] 
data.flights <- data.flights[data.flights$DESTINATION_AIRPORT != "",] 

##################################################################################################

##################################################################################################
# 1-D Plots
#################################################################################################

# Departure Delay by Flight
boxplot(data.flights$DEPARTURE_DELAY~data.flights$AIRLINE, main = "Departure Delay")

# Arrival Delay by Flight
boxplot(data.flights$ARRIVAL_DELAY~data.flights$AIRLINE, main = "Arrival Delay")

# Flights per year
par(mar = c(2,2,2,2))
fpy <- data.frame(tapply(data.flights$AIRLINE,data.flights$AIRLINE,length))
barplot(fpy$tapply.data.flights.AIRLINE..data.flights.AIRLINE..length.,names.arg = rownames(fpy)
        , main= "Flights per year", ylab = "# Flights", horiz = T)


pie(fpy$tapply.data.flights.AIRLINE..data.flights.AIRLINE..length., labels = rownames(fpy)
    , main = "Number of Flights")

airlines_Data = merge(airlines,flight.data,'AIRLINE')

# Delay by Month
boxplot(data.flights$ARRIVAL_DELAY~data.flights$MONTH, main = "Delay By Month")
delay_from_departure = data.flights[data.flights$DEPARTURE_DELAY > 0,]
dim <- data.frame(tapply(delay_from_departure$DEPARTURE_DELAY,delay_from_departure$MONTH,length))
barplot(dim$tapply.delay_from_departure.DEPARTURE_DELAY..delay_from_departure.MONTH..
        , names.arg = rownames(dim)
        , main = "# Delay by Month")
#Arrival
delay_from_departure = data.flights[data.flights$ARRIVAL_DELAY > 0,]
dim <- data.frame(tapply(delay_from_departure$ARRIVAL_DELAY,delay_from_departure$MONTH,length))
barplot(dim$tapply.delay_from_departure.ARRIVAL_DELAY..delay_from_departure.MONTH..
        , names.arg = rownames(dim)
        , main = "# Delay by Month")


# Delay by Day
dim_day <- data.frame(tapply(delay_from_departure$ARRIVAL_TIME,delay_from_departure$DAY_OF_WEEK,length))
barplot(dim_day$tapply.delay_from_departure.ARRIVAL_TIME..delay_from_departure.DAY_OF_WEEK..
        , names.arg = rownames(dim_day)
        , main = "# Delay by Day")

colnames(airports) <- c("ORIGIN_AIRPORT" , "AIRPORT" ,  "CITY"   ,   "STATE"   ,  "COUNTRY"  , "LATITUDE"  , "LONGITUDE")
airport_state = merge(data.flights,airports, "ORIGIN_AIRPORT")
#airline_airport <- data.frame(tapply(airport_state$DEPARTURE_DELAY,list(airport_state$CITY,airport_state$AIRLINE),mean))
#View(airline_airport)

##########################################################
# Delay from each airport by different airlines
#########################################################
# Origin
summary(airport_state)
airport_state = airport_state[!is.na(airport_state$DEPARTURE_DELAY),]
trial <- data.frame(aggregate(airport_state$DEPARTURE_DELAY,list(airport_state$CITY,airport_state$AIRLINE),mean))
trial[is.na(trial$x),3] <- 0
trial <- trial[trial$x <100,  ]
trial <- trial[trial$x > 10,  ]
g1 = ggplot(trial, aes(x = Group.2,
                            y = trial$Group.1, fill = trial$x)) + geom_tile() + scale_fill_gradient(low="mediumspringgreen", high= "orangered") + theme(text = element_text(size = 7)) + ggtitle("Delay in Departure from Source Airport")

# Destination
colnames(airports) <- c("DESTINATION_AIRPORT" , "AIRPORT" ,  "CITY"   ,   "STATE"   ,  "COUNTRY"  , "LATITUDE"  , "LONGITUDE")
airport_state_Des = merge(data.flights,airports, "DESTINATION_AIRPORT")
airport_state_Des = airport_state_Des[!is.na(airport_state_Des$ARRIVAL_DELAY),]

trial2 <- data.frame(aggregate(airport_state_Des$ARRIVAL_DELAY,list(airport_state_Des$CITY,airport_state_Des$AIRLINE),mean))
trial2[is.na(trial2$x),3] <- 0
trial2 <- trial2[trial2$x > 10,  ]
g2 <- ggplot(trial2, aes(x = Group.2,
                       y = trial2$Group.1, fill = trial2$x)) + geom_tile() + scale_fill_gradient(low="mediumspringgreen", high= "orangered") + theme(text = element_text(size = 7)) + ggtitle("Delay in Arrival at Destination Airport")

grid.arrange(arrangeGrob(g1,g2,ncol=2,nrow = 1))



# Flights from Airport
colnames(airports)
colnames(airports) <- c("DESTINATION_AIRPORT" , "AIRPORT" ,  "CITY"   ,   "STATE"   ,  "COUNTRY"  , "LATITUDE"  , "LONGITUDE")
destinat <- merge(data.flights,airports,"DESTINATION_AIRPORT")
colnames(destinat) <- c( "DESTINATION_CODE","YEAR","MONTH","DAY","DAY_OF_WEEK","AIRLINE","FLIGHT_NUMBER",      
                         "TAIL_NUMBER","ORIGIN_AIRPORT","SCHEDULED_DEPARTURE","DEPARTURE_TIME","DEPARTURE_DELAY","TAXI_OUT","WHEELS_OFF",         
                         "SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME","DISTANCE","WHEELS_ON","TAXI_IN","SCHEDULED_ARRIVAL",  
                         "ARRIVAL_TIME","ARRIVAL_DELAY","DIVERTED","CANCELLED","CANCELLATION_REASON","AIR_SYSTEM_DELAY","SECURITY_DELAY",     
                         "AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY","D_AIRPORT","D_CITY","D_STATE","COUNTRY",            
                         "D_LATITUDE","D_LONGITUDE" )

colnames(airports) <- c("ORIGIN_AIRPORT","AIRPORT","CITY","STATE","COUNTRY","LATITUDE","LONGITUDE")

origi_dest <- merge(destinat,airports,"ORIGIN_AIRPORT")

colnames(origi_dest) <- c("ORIGIN_CODE","DESTINATION_CODE","YEAR","MONTH","DAY","DAY_OF_WEEK","AIRLINE","FLIGHT_NUMBER",      
                          "TAIL_NUMBER","SCHEDULED_DEPARTURE","DEPARTURE_TIME","DEPARTURE_DELAY","TAXI_OUT","WHEELS_OFF",         
                          "SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME","DISTANCE","WHEELS_ON","TAXI_IN","SCHEDULED_ARRIVAL",  
                          "ARRIVAL_TIME","ARRIVAL_DELAY","DIVERTED","CANCELLED","CANCELLATION_REASON","AIR_SYSTEM_DELAY","SECURITY_DELAY",     
                          "AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY","D_AIRPORT","D_CITY","D_STATE","COUNTRY",            
                          "D_LATITUDE","D_LONGITUDE","O_AIRPORT","O_CITY","O_STATE","COUNTRY.y","O_LATITUDE","O_LONGITUDE" )

WN_Airport <- origi_dest[origi_dest$AIRLINE == "WN",]

#WN_Airport <- WN_Airport[1:1000,]

try = aggregate(origi_dest$ORIGIN_CODE ,list(origi_dest$O_LATITUDE,origi_dest$O_LONGITUDE),length)
try2 = aggregate(origi_dest$DESTINATION_CODE ,list(origi_dest$D_LATITUDE,origi_dest$D_LONGITUDE),length)

usa <- borders("state", colour="black", fill="white") # create a layer of borders
ggplot() + usa + 
  geom_point(data=try, aes(x = Group.2, y = try$Group.1, size = x), col = "orange")+
  geom_point(data=try2, aes(x = Group.2, y = Group.1, size = x), col = "blue", alpha = 0.3)+
  #  geom_text_repel(data=airports, aes(x = airports$LONGITUDE, y = airports$LONGITUDE, label = "airport"), col = "black", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="lightblue"),  
        axis.line = element_blank(),
#        axis.text.x = element_blank(),
#        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
        ) + ylim(25,50) + xlim(-125,-67)+ ggtitle("Origin and Departure")

#################################################################################
# Number of Flights to Delay
################################################################################

flights <- data.frame(tapply(data.flights$AIRLINE,data.flights$AIRLINE, length))
flights$airlines <- rownames(flights)

data.flights <- data.flights[!is.na(data.flights$DEPARTURE_DELAY),]
Departure_delay <- data.frame(tapply(data.flights$DEPARTURE_DELAY,data.flights$AIRLINE,mean))
Departure_delay$airlines <- rownames(Departure_delay)

adjun = merge(flights,Departure_delay,"airlines")

data.flights <- data.flights[!is.na(data.flights$ARRIVAL_DELAY),]
Arrival_Delay <- data.frame(tapply(data.flights$ARRIVAL_DELAY,data.flights$AIRLINE,mean))
Arrival_Delay$airlines <- rownames(Arrival_Delay)


final = merge(adjun,Arrival_Delay,"airlines")
colnames(final) <- c("airlines","Total_Flights","Departure_Delay","Arrival_Delay")
rownames(final) <- final$airlines
final <- final[,c(-1,-2)]
barplot(t(final), beside = T, ylim = c(0,20), legend = T, main = "Delay By Flights")

plot(final, type = 'h')


plot(final)
par(mar = c(3,3,3,5))
barplot(final$Total_Flights, names.arg = final$airlines, main = "Number of Flights to Delay")

par(new=TRUE)

plot(final[,3], xlab="", ylab="", ylim=c(0,20), 
     axes=FALSE, type="o", col="orange")

par(new=TRUE)

plot(final[,4], xlab="", ylab="", ylim=c(0,20), 
     axes=FALSE, type="o", col="blue")

mtext("Delay",side=4,col="blue",line=4) 
axis(4, ylim=c(0,20), col="blue",col.axis="orange",las=1)

legend("topleft",legend=c("Departure Delay","Arrival Delay"),
       text.col=c("orange","blue"),pch=c(16,16),col=c("orange","blue"))


###############################################################################################
###############################################################################################

###############################################################################################
#Origin #Destination #Delay
###############################################################################################


origin = data.frame(tapply(data.flights$ORIGIN_AIRPORT, data.flights$ORIGIN_AIRPORT, length))
origin$source <-  rownames(origin)


des <- data.frame(tapply(data.flights$DESTINATION_AIRPORT, data.flights$DESTINATION_AIRPORT, length))
des$source <-  rownames(des)


mid <- merge(origin,des,"source")


deps <- data.frame(tapply(data.flights$DEPARTURE_DELAY,data.flights$ORIGIN_AIRPORT, mean))
deps$source <- rownames(deps)

semfi <- merge(mid,deps,"source")


arrdes <- data.frame(tapply(data.flights$ARRIVAL_DELAY,data.flights$DESTINATION_AIRPORT, mean))
arrdes$source <- rownames(arrdes)


final <- merge(semfi,arrdes,"source")

head(final,5)

colnames(final) <- c("Airport","Outgoing_Flights","Incoming_Flights", "Departure_Delay","Arrival_Delay")

sorted_flights <- final[order(final$Outgoing_Flights,decreasing = T),]

sorted_opp <- final[order(final$Outgoing_Flights,decreasing = F),]


busy_airport <- head(sorted_flights,15)


new_m <- busy_airport[,-c(4,5)]
rownames(new_m) <- new_m[,1]
new_m <- new_m[,-c(1)]
newm_t <- t(new_m)

#######################
# Busiest Airports
######################

par(mar = c(3,3,3,5))
barplot(newm_t,beside = T, ylim = c(0,350000), legend.text = c("Outgoing","Incoming")
        , main = "Busyness to Delay")

par(new=TRUE)

plot(busy_airport[,4], xlab="", ylab="", ylim=c(0,20), 
     axes=FALSE, type="o", col="orange")

par(new=TRUE)

plot(busy_airport[,5], xlab="", ylab="", ylim=c(0,20), 
     axes=FALSE, type="o", col="blue")

mtext("Delay",side=4,col="blue",line=4) 
axis(4, ylim=c(0,20), col="blue",col.axis="orange",las=1)

legend("center",legend=c("Departure Delay","Arrival Delay"),
       text.col=c("orange","blue"),pch=c(16,16),col=c("orange","blue"))


#####################################
# Slowest Airports
####################################

sorted_opp <- head(sorted_opp,15)
new_m2 <- sorted_opp[,-c(4,5)]
rownames(new_m2) <- new_m2[,1]
new_m2 <- new_m2[,-c(1)]
newm_t2 <- t(new_m2)

par(mar = c(3,3,3,5))
barplot(newm_t2,beside = T, ylim = c(0,250), legend.text = c("Outgoing","Incoming")
        , main = "Slowness to Delay")

par(new=TRUE)

plot(sorted_opp[,4], xlab="", ylab="", ylim=c(-10,50), 
     axes=FALSE, type="o", col="orange")

par(new=TRUE)

plot(sorted_opp[,5], xlab="", ylab="", ylim=c(-10,50), 
     axes=FALSE, type="o", col="blue")

mtext("Delay",side=4,col="blue",line=4) 
axis(4, ylim=c(-10,50), col="blue",col.axis="orange",las=1)

legend("topleft",legend=c("Departure Delay","Arrival Delay"),
       text.col=c("orange","blue"),pch=c(16,16),col=c("orange","blue"))

####################################################################################################################

###################################################################################################################
# Network of flight with highest Delay
##################################################################################################################

highest_delay <- data.flights[data.flights$AIRLINE == "NK",]
highest_delay <- highest_delay[!is.na(highest_delay$ARRIVAL_DELAY),]
mean(highest_delay$ARRIVAL_DELAY)
#highest_delay$ARRIVAL_DELAY <-  0 + (((highest_delay$ARRIVAL_DELAY - min(highest_delay$ARRIVAL_DELAY))*60)/
#  (max(highest_delay$ARRIVAL_DELAY) - min(highest_delay$ARRIVAL_DELAY)))

edgelist <- as.matrix(highest_delay[c("ORIGIN_AIRPORT", "DESTINATION_AIRPORT")])
a <-(data.frame(tapply(highest_delay$ARRIVAL_DELAY,list(highest_delay$ORIGIN_AIRPORT,highest_delay$DESTINATION_AIRPORT),mean)))
#summary(highest_delay$ARRIVAL_DELAY)

a[is.na(a)] <- 0 

#tapply(highest_delay$ORIGIN_AIRPORT,highest_delay$ORIGIN_AIRPORT,length)

#a <- apply(a, 1, fun)
#View(a)
a <- as.matrix(a)
diag(a) <- 0

g <- graph.adjacency(a, mode="undirected", weighted=TRUE)
plot(g)


#g <- graph_from_edgelist(edgelist, directed = TRUE)
#g <- simplify(g)
#par(mar=rep(0,4))
#plot.igraph(g, 
#            edge.arrow.size=0,
#            edge.color="black",
#            edge.curved=TRUE,
#            edge.width=2,
#            vertex.size=3,
#            vertex.color=NA, 
#            vertex.frame.color=NA, 
#            vertex.label=V(g)$name,
#            vertex.label.cex=1,
#            layout=layout.fruchterman.reingold
#)
#l <- layout.sphere(g)
l <- layout.circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot(g)

#plot.igraph(g,edge.arrow.size = 0
#            , edge.arrow.width = 0
#            , main = "Info Viz Social Graph")

E(g)$width <- E(g)$weight/max(E(g)$weight) * 2 + 1
plot(g)



out.deg <- degree(g, mode = "out")
size <- out.deg
size <- size/max(size)
size <- 2 + (6 * size)
V(g)$size <- size
par(mar = c(1,1,1,1))
plot(g)

#in.deg <- degree(g, mode = "in")
out.deg <- degree(g, mode = "out")
par(mar = c(4,12,4,2))
barplot(sort(out.deg), horiz = T, main = "Going Out"
        , col = "brown", las = 2)
in.deg <- degree(g, mode = "in")
par(mar = c(4,12,4,2))
barplot(sort(in.deg), horiz = T, main = "Poupular"
        , col = "forestgreen", las = 2)



#plot.igraph(g,edge.arrow.size = 0
#            , edge.arrow.width = 0
#            , main = "Info Viz Social Graph")
#
V(g)$color <- "lightcyan3"
E(g)$color <- "grey78"
E(g)[width>mean(E(g)$width)]$color <- '#1A1A75'
#E(g)[from("FLL")]$color <- 'orange3'
E(g)[from("ORD") & width>mean(E(g)$width)]$color <- '#FC7315'
#E(g)[from("FLL")]$width <- 1
plot(g, main = "Most Flights with highest Delay")
#plot.igraph(g,edge.arrow.size = 0
#           , edge.arrow.width = 0
#            , main = "Info Viz Social Graph")
nrow(data.flights)
####################################################################################################
# Heat Map
####################################################################################################
summary(airport_state)
airport_state = airport_state[airport_state$AIRLINE == "NK",]
airport_state = airport_state[!is.na(airport_state$DEPARTURE_DELAY),]
trial <- data.frame(aggregate(airport_state$DEPARTURE_DELAY,list(airport_state$CITY,airport_state$AIRLINE),mean))
trial[is.na(trial$x),3] <- 0
trial <- trial[trial$x <100,  ]
trial <- trial[trial$x > 10,  ]
trial <- trial[order(trial$x),]
trial$Group.1 = factor(trial$Group.1, levels = trial$Group.1[order(trial$x)])
g1 = ggplot(trial, aes(x = Group.2,
                       y = trial$Group.1, fill = trial$x)) + geom_tile() + scale_fill_gradient(low="#FC7315", high= "#1A1A75") + theme(text = element_text(size = 7)) + ggtitle("Delay in Departure from Source Airport")

# Destination
colnames(airports) <- c("DESTINATION_AIRPORT" , "AIRPORT" ,  "CITY"   ,   "STATE"   ,  "COUNTRY"  , "LATITUDE"  , "LONGITUDE")
airport_state_Des = merge(data.flights,airports, "DESTINATION_AIRPORT")
airport_state_Des = airport_state_Des[!is.na(airport_state_Des$ARRIVAL_DELAY),]
airport_state_Des = airport_state_Des[airport_state_Des$AIRLINE == "NK",]

trial2 <- data.frame(aggregate(airport_state_Des$ARRIVAL_DELAY,list(airport_state_Des$CITY,airport_state_Des$AIRLINE),mean))
trial2[is.na(trial2$x),3] <- 0
trial2 <- trial2[trial2$x > 10,  ]
trial2 <- trial2[order(trial2$x),]
rownames(trial2) <- c(1:nrow(trial2))
trial2$Group.1 = factor(trial2$Group.1, levels = trial2$Group.1[order(trial2$x)])
g2 <- ggplot(trial2, aes(x = Group.2,
                         y = Group.1, fill = trial2$x)) + geom_tile() + scale_fill_gradient(low="#FC7315", high= "#1A1A75") + theme(text = element_text(size = 7)) + ggtitle("Delay in Arrival at Destination Airport")

grid.arrange(arrangeGrob(g1,g2,ncol=2,nrow = 1))

outgoing = length(which(highest_delay$ORIGIN_AIRPORT == "ORD"))
incoming = length(which(highest_delay$DESTINATION_AIRPORT == "ORD"))



  
##################################################################################################################

#barplot(tapply(highest_delay$CANCELLED,highest_delay$,length))
plot(density(highest_delay$DISTANCE), main = "Distance Covered by Spirit Airlines")




##################################################################################################################
# States With Highest Delay
##################################################################################################################
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}


origi_dest<- origi_dest[!is.na(origi_dest$ARRIVAL_DELAY),]
st_delay <- data.frame(tapply(origi_dest$ARRIVAL_DELAY,origi_dest$D_STATE,mean))
st_delay$state <- rownames(st_delay)
colnames(st_delay) <- c("Arrival_delay", "STATE")

usa <- borders("state", colour="black", fill=st_delay$Arrival_delay) # create a layer of borders
ggplot() + usa + 
  geom_point(x = airports$LONGITUDE ,y=airports$LATITUDE, color = "black")+
  #geom_point(data=try2, aes(x = Group.2, y = Group.1, size = x), col = "blue", alpha = 0.3)+
  #  geom_text_repel(data=airports, aes(x = airports$LONGITUDE, y = airports$LONGITUDE, label = "airport"), col = "black", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="lightblue"),  
        axis.line = element_blank(),
        #        axis.text.x = element_blank(),
        #        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) + ylim(25,50) + xlim(-125,-67)+ ggtitle("Origin and Departure")






us <- map_data("state")
#airports$STATE <- tolower(airports$STATE)

#merge(data.flights,airports, by = '')
unique(nt$D_STATE)

st_delay$state <- abb2state(st_delay$state)
st_delay$state <- tolower(st_delay$state)
us$region
i <- ggplot(st_delay,aes(map_id = state))
i <- i + geom_map(map=us,aes(fill=Arrival_delay), color = "black")
#i <- i + geom_point(x = airports$LONGITUDE ,y=airports$LATITUDE, color = "black")
#i <- i + geom_point(data=try, aes(x = Group.2, y = try$Group.1, size = x), col = "#970027")
#i <- i + geom_point(data=try2, aes(x = Group.2, y = Group.1, size = x), col = "#F0E68C", alpha = 0.3)
#i <- i + geom_curve(aes(x = O_LONGITUDE, y = O_LATITUDE, xend = D_LONGITUDE, yend = D_LATITUDE), col = "black", size = 1, curvature = .2) 
i <- i + expand_limits(x = us$long,y=us$lat)
i <- i + coord_map() + ggtitle("") + scale_fill_gradient(low = "white", high = "blue")
i

airports$STATE <- abb2state(airports$STATE)
airports$STATE <- tolower(airports$STATE)

par(new=TRUE)
new <- merge(airports,st_delay,"STATE")
#colnames(final) <- c("Airport","Outgoing_Flights","Incoming_Flights", "Departure_Delay","Arrival_Delay")
colnames(new) <- c("STATE","Airport","AIRPORT","CITY",
                   "COUNTRY","LATITUDE","LONGITUDE","Arrival_delay_airport")

new_a <- merge(new,final,"Airport")
  
i <- ggplot(new_a,aes(map_id = STATE))
i <- i + geom_map(map=us,aes(fill = Arrival_delay_airport), color = "black")
i <- i + geom_point(x = new_a$LONGITUDE ,y=new_a$LATITUDE, color = "#CD6C95", alpha = 0.5, size = new_a$Arrival_Delay)
#i <- i + geom_point(data=try, aes(x = Group.2, y = try$Group.1, size = x), col = "#970027")
#i <- i + geom_point(data=try2, aes(x = Group.2, y = Group.1, size = x), col = "#F0E68C", alpha = 0.3)
#i <- i + geom_curve(aes(x = O_LONGITUDE, y = O_LATITUDE, xend = D_LONGITUDE, yend = D_LATITUDE), col = "black", size = 1, curvature = .2) 
i <- i + expand_limits(x = us$long,y=us$lat)
i <- i + coord_map() + ggtitle("Delay at Individual Airport to Overall State") + scale_fill_gradient(low = "white", high = "blue")
i
