#
# Ian Ustanik
# Final Project
#
# Some Visualizations Inspired By Data-To-Viz 

# Data Preparation
data.dir <- "C:\\Users\\ian_u\\OneDrive\\Documents\\IST 719\\Homeworks\\"
data.dir2 <- "C:\\Users\\ian_u\\OneDrive\\Documents\\IST 719\\Final Project\\"
imag.dir <- "C:\\Users\\ian_u\\OneDrive\\Documents\\IST 719\\Labs\\Data\\"
transportation <- read.csv(file = paste0(data.dir, "Transportation_and_Health_Tool.csv"), header = TRUE, stringsAsFactors = FALSE)
# https://www.transportation.gov/transportation-health-tool/indicators
# Dataset score (31 columns*4) * (382 rows/100) = 473.68
health <- read.csv(file = paste0(data.dir2, "AmericanHumanDevelopment.csv"), header = TRUE, stringsAsFactors = FALSE)
# https://measureofamerica.org/maps/?state^health^all_all^HDI^hdi

# NY Public Transit Barplot
NY <- transportation[grep("NY", transportation$Metropolitan.Statistical.Area),]
NY <- NY[-nrow(NY),]
NY

sort(NY$Commute.Mode.Share...Transit..Raw.Value, decreasing = FALSE, na.last = TRUE)
NY$Commute.Mode.Share...Transit..Raw.Value <- as.numeric(sub("%", "", NY$Commute.Mode.Share...Transit..Raw.Value))
train <- readJPEG(paste0(imag.dir, "train-min.jpg"))
# https://en.wikipedia.org/wiki/File:MTA_NYC_Subway_Q_train_at_96th_St.jpg

library(ggplot2)
library(jpeg)
library(grid)

ggplot(NY, aes(x = reorder(Metropolitan.Statistical.Area, - Commute.Mode.Share...Transit..Raw.Value), y = Commute.Mode.Share...Transit..Raw.Value)) +
  annotation_custom(rasterGrob(train, 
                               width = unit(1.25,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_bar(stat = "identity", fill = c("#593d9cff", "#6b4596ff", "#7e4e90ff", "#90548bff", "#a65c85ff", "#b8627dff", "#cc6a70ff", "#de7065ff", "#eb8055ff", "#f68f46ff", "#f9a242ff")) +
         coord_flip()

# Syracuse Transit Methods Pie Chart
Syracuse <- transportation[(transportation$Metropolitan.Statistical.Area == "Syracuse, NY"),]     
Syracuse

Syracuse$Commute.Mode.Share...Auto..Raw.Value <- as.numeric(sub("%", "", Syracuse$Commute.Mode.Share...Auto..Raw.Value))/100
Syracuse$Commute.Mode.Share...Transit..Raw.Value <- as.numeric(sub("%", "", Syracuse$Commute.Mode.Share...Transit..Raw.Value))/100
Syracuse$Commute.Mode.Share...Bicycle..Raw.Value <- as.numeric(sub("%", "", Syracuse$Commute.Mode.Share...Bicycle..Raw.Value))/100
Syracuse$Commute.Mode.Share...Walk..Raw.Value <- as.numeric(sub("%", "", Syracuse$Commute.Mode.Share...Walk..Raw.Value))/100

par(mar=c(4, 4, 4, 4))
slices <- c(Syracuse$Commute.Mode.Share...Auto..Raw.Value, Syracuse$Commute.Mode.Share...Transit..Raw.Value, Syracuse$Commute.Mode.Share...Bicycle..Raw.Value, Syracuse$Commute.Mode.Share...Walk..Raw.Value)
labels <- c("Auto", "Public Transit", "Bicycle", "Walk")
percents <- round(slices/sum(slices)*100)
labels <- paste(labels, percents)
labels <- paste(labels,"%",sep="")
pie(slices, labels, col= c("#593d9cff", "#90548bff", "#cc6a70ff", "#eb8055ff"), main="Syracuse, New York Commuter Methods Breakdown")

# DUI Scatterplot
transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value <- as.numeric(transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value)
transportation$Commute.Mode.Share...Auto..Raw.Value <- as.numeric(sub("%", "", transportation$Commute.Mode.Share...Auto..Raw.Value))

Commute.Mode.Share...Auto..Raw.Value <- transportation[!is.na(transportation$Commute.Mode.Share...Auto..Raw.Value), ]
DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value <- transportation[!is.na(transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value), ]

plot(transportation$Commute.Mode.Share...Auto..Raw.Value, transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value, xlim = c(0,100), xlab = "Auto Commuters per 100", ylab = "Fatalities per 10,000 Residents", main = "Auto Commuters vs DUI Deaths", pch = 17, col = "blue", cex = .8)
rug(transportation$Commute.Mode.Share...Auto..Raw.Value, side = 3, col = "orange")
rug(transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value, side = 2, col = "purple")
abline(lm(transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value ~ transportation$Commute.Mode.Share...Auto..Raw.Value), col = "red")
abline(h = mean(transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value, na.rm = TRUE), col = "red", lty = 3)
abline(v = mean(transportation$Commute.Mode.Share...Auto..Raw.Value, na.rm = TRUE), col = "red", lty = 3)

mean(transportation$DUI.DWI.Fatalities.per.10.000.Residents..Raw.Value, na.rm = TRUE)
mean(transportation$Commute.Mode.Share...Auto..Raw.Value, na.rm = TRUE)

# Auto Score Heatmap
States <- read.csv(file = paste0(data.dir, "States.csv"), header = TRUE, stringsAsFactors = FALSE)
States

States <- States[-nrow(States),]

States
library(maps)
m <- map("state")
m

match.map(database = "state", regions = States$State, exact = FALSE, warn = TRUE)
new_df <- States[,c("State", "Commute.Mode.Share...Auto..Score")]

colnames(new_df) <- c("state", "auto score")

library(viridis)
num.cols <- 10
my.color.vec <- rev(magma(num.cols))
pie(rep(1, length(my.color.vec)), col = my.color.vec)

new_df$`auto score`

new2 <- new_df$`auto score`-5
col.index <- 1 + (9 * new2/max(new2))
col.index <- round(col.index, 0)
col.index
new_df$col.index <- col.index
new_df$color <- my.color.vec[new_df$col.index]
num.cats = 10

library(mapproj)
state.order <- match.map(database = "state", regions = States$State, exact = FALSE, warn = TRUE)
map("state", col = new_df$color[state.order], fill = TRUE, resolution = 0, lty = 1, border = TRUE, projection = "polyconic")
library(plotfunctions)
gradientLegend(new_df$`auto score`, color = my.color.vec, nCol = 10, side = 1, length = 1, pos.num = 1, inside = FALSE)

# Transit Score Lollipop
library(dplyr)

States <- States %>% 
  mutate(mycolor = ifelse(Commute.Mode.Share...Transit..Score>mean(Commute.Mode.Share...Transit..Score), "#9F2F7F", "#f9a242ff"))

ggplot(States, aes(x = State, y = Commute.Mode.Share...Transit..Score)) +
  geom_segment(aes(x = reorder(State, Commute.Mode.Share...Transit..Score), xend = State, y = mean(Commute.Mode.Share...Transit..Score), yend = Commute.Mode.Share...Transit..Score), color = States$mycolor) +
  geom_point(color = States$mycolor, size = 4, alpha = 0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# State Expenditures Circular Bargraph
h$'State Expenditure on Transportation ($ per person)'<- as.numeric(h$'State Expenditure on Transportation ($ per person)')

h[2,62] <- 1000 #(3230)
h[50,62] <- 999 #(1078)

label_data <- h

number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (1:50-0.5) /number_of_bar     

label_data$hjust<-ifelse( angle < -90, 1, 0)

label_data$angle<-ifelse(angle < -90, angle+180, angle)

library(ggforce)

p <- ggplot(h, aes(x = State, y = `State Expenditure on Transportation ($ per person)`)) + 
  

  geom_bar(stat = "identity", fill = alpha("darkorange", 0.7)) +

  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),    
  ) +

  coord_polar(start = 0) +
  
  geom_text(data = label_data, aes(x = State, y = `State Expenditure on Transportation ($ per person)` + 10, label = State, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE ) 
p

# Transportation and Health Scatterplot
health <- health[-(1:9), , drop = FALSE]
names(health) <- health[1,]
health <- health[-1,]
h <- health[2:52,]
h <- h[-9,]
h <- h[,1:62]

names(h)[13] <- "healthindex"
h$healthindex <- as.numeric(h$healthindex)

plot(States$Commute.Mode.Share...Transit..Score, h$healthindex, xlab = "Transit Score", ylab = "Health Index", xlim = c(0,100), ylim = c(0,7), main = "Transit Score vs State Health Index", pch = 17, col = "blue", cex = 2)
rug(States$Commute.Mode.Share...Transit..Score, side = 3, col = "orange")
rug(h$healthindex, side = 2, col = "purple")
abline(lm(h$healthindex ~ States$Commute.Mode.Share...Transit..Score), col = "red")
abline(h = mean(h$healthindex, na.rm = TRUE), col = "red", lty = 3)
abline(v = mean(States$Commute.Mode.Share...Transit..Score, na.rm = TRUE), col = "red", lty = 3)

# Transit Score Heat Map
States
library(maps)
m <- map("state")
m

match.map(database = "state", regions = States$State, exact = FALSE, warn = TRUE)
cool_df <- States[,c("State", "Commute.Mode.Share...Transit..Score")]

colnames(cool_df) <- c("state", "transit score")

library(viridis)
num.cols <- 10
my.color.vec <- rev(magma(num.cols))
pie(rep(1, length(my.color.vec)), col = my.color.vec)

cool_df$`transit score`

cool2 <- cool_df$`transit score`-16
col.index <- 1 + (9 * cool2/max(cool2))
col.index <- round(col.index, 0)
col.index
cool_df$col.index <- col.index
cool_df$color <- my.color.vec[cool_df$col.index]
num.cats = 10

library(mapproj)
state.order <- match.map(database = "state", regions = States$State, exact = FALSE, warn = TRUE)
map("state", col = cool_df$color[state.order], fill = TRUE, resolution = 0, lty = 1, border = TRUE, projection = "polyconic")
library(plotfunctions)
gradientLegend(cool_df$`transit score`, color = my.color.vec, nCol = 10, side = 1, length = 1, pos.num = 1, inside = FALSE)

# Health Index Heat Map
h
library(maps)
m <- map("state")
m

match.map(database = "state", regions = h$State, exact = FALSE, warn = TRUE)
boogie_df <- h[,c("State", "healthindex")]

colnames(boogie_df) <- c("state", "health index")

library(viridis)
num.cols <- 10
my.color.vec <- rev(magma(num.cols))
pie(rep(1, length(my.color.vec)), col = my.color.vec)

boogie_df$`health index`

boogie2 <- boogie_df$`health index`-3.73
col.index <- 1 + (9 * boogie2/max(boogie2))
col.index <- round(col.index, 0)
col.index
boogie_df$col.index <- col.index
boogie_df$color <- my.color.vec[boogie_df$col.index]
num.cats = 10

library(mapproj)
state.order <- match.map(database = "state", regions = h$State, exact = FALSE, warn = TRUE)
map("state", col = boogie_df$color[state.order], fill = TRUE, resolution = 0, lty = 1, border = TRUE, projection = "polyconic")
library(plotfunctions)
gradientLegend(boogie_df$`health index`, color = my.color.vec, nCol = 10, side = 1, length = 1, pos.num = 1, inside = FALSE)

