#set working directory

#clear active working environment
rm(list=ls())

#load working libraries
library(tidyverse)
library(magrittr)
library(reshape2)
library(readxl)
library(openxlsx)
library(networkD3)

#load data set
data <- read.xlsx("./2020/Totals.xlsx",9)

#assign color values - Colors were chosen at random and can be amended if necessary by changing hex code
#tax type colors first
ttc <- data.frame(
          taxtype = c("Service Revenue","Local Tax","Local Non-Tax Revenue","Intergovernmental Revenue","Proceeds & Transfers In","Property Tax", "Grant"),
          color= c("#5bdbff", "#ff6255", "#ffa755", "#9e66ff", "#0590b8", "#FF1300","#ff7a00")
      )
#essentially a vlookup
data <- merge(data,ttc,by.x = "Tax.Type", by.y = "taxtype", all = TRUE)
colnames(data)[11] <- "TaxTypeColor"
#fund category colors
fcc <- data.frame(
          fcc = c("Corporate Funds","Enterprise Funds","Special Revenue Funds","Debt Service Funds","Pension Trust Funds", "TIF Admin Funds","Grant Funds"),
          color= c("#752df3", "#a2ebff", "#ffa29b", "#ff8719", "#bf9ff7", "#1fc3f1", "FFFFFF")
      )
data <- merge(data,fcc,by.x = "Fund.Category", by.y = "fcc", all = TRUE)
colnames(data)[12] <- "FundCategoryColor"
#PRODUCTS 1-4 - REVENUE INFORMATION

#Product 1 Color Map
p1c <- data.frame(
  ttc= c(data$Tax.Type),
  color= c(as.character(data$TaxTypeColor))
)
p1c <- unique(p1c)
p1ca <- paste(p1c$ttc)
p1cb <- paste(p1c$color)
p1c <- setNames(p1cb, p1ca)

#Product 2 Color Map
p2c <- data.frame(
  ttc= c(data$CTBA.Category),
  color= c(as.character(data$TaxTypeColor))
)
p2c <- unique(p2c)
p2ca <- paste(p2c$ttc)
p2cb <- paste(p2c$color)
p2c <- setNames(p2cb, p2ca)

#PRODUCT NUMBER 1 - Tax Type - Stacked Bar Chart - Color Coded by Tax Type
#set chart type and labels and scales
a <- ggplot(data, aes(x=Year,y=Expected.Amount, fill=Tax.Type)) +
  geom_bar(stat = "identity", position = position_stack()) + 
  labs(x = "Year", y= "Expected Amount", title = "Revenue Sources - 2018") +
  theme_grey() + 
  scale_x_continuous(limits = c(2017, 2019), breaks = 2018) + 
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
#group fill colors to plot
  scale_fill_manual(values=p1c)
#product
a

#PRODUCT NUMBER 2 - CTBA Categories - Stacked Bar Chart - Color Coded By Tax Type
#set chart type and labels and scales
b <- ggplot(data, aes(x=`Year`,y=`Expected Amount`,group=`Tax Type`,fill=`CTBA Category`)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(x = "Year", y= "Expected Amount", title = "Revenue Sources - 2018") + 
  theme_grey() + 
  scale_x_continuous(limits = c(2017, 2019), breaks = 2018) + 
  scale_y_continuous(label = scales::dollar) + 
#group fill colors to plot
  scale_fill_manual(values=p2c)
b

#=========================================================================================================
# GG PLOTS COMPLETED - TIME TO CONDENSE DATA FOR BETTER GRAPHICAL REPRESENTATIONS

#CONDENSE DATA
data <- data %>% group_by(Fund.Category,Tax.Type,CTBA.Fund.Category,TaxTypeColor,FundCategoryColor) %>%
  summarize(Expected.Amount= sum(Expected.Amount))

#PRODUCT NUMBER 3 - Tax Type into Fund Category - Sankey Diagram - Color Coded by Tax Type & Fund Category

#sankey diagram
links=data
nodes=data.frame(name=c(as.character(links$Tax.Type), as.character(links$Fund.Category)) %>% unique())
links$IDsource=match(links$Tax.Type, nodes$name)-1
links$IDtarget=match(links$Fund.Category, nodes$name)-1
#Product 3 Color Map
p3c <- data.frame(
  name=unique(c(as.character(links$Tax.Type),as.character(links$Fund.Category))),
  color=unique(c(as.character(links$TaxTypeColor),as.character(links$FundCategoryColor)))
)

#colors assigned by node name into javascript - this works but is untidy - HEX CODES WORK
nodes$group <- as.character(c(1:length(nodes$name)))
test <- paste('"', p3c$color, '"', sep = "", collapse = ',')
nodelist <- paste('"', nodes$group, '"', sep = "", collapse = ",") 
mycolor <- paste0('d3.scaleOrdinal() .domain([', nodelist, ']) .range([', test, '])')
#sankey continued
c <- sankeyNetwork(Links = links, Nodes = nodes,
                            Source = "IDsource", Target = "IDtarget",
                            Value = "Expected.Amount", NodeID = "name",
                            sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                            nodePadding=30, nodeWidth = 30, colourScale = noquote(mycolor), NodeGroup = "group")
c

saveNetwork(c, "Product3.html")

#PRODUCT NUMBER 4 - Tax Type into CTBA Fund Category - Sankey Diagram - Color Coded by Tax Type & Fund Category
#Product 4 Color Map
#sankey diagram
links2=data
nodes2=data.frame(name=c(as.character(links2$Tax.Type), as.character(links2$CTBA.Fund.Category)) %>% unique())
links2$IDsource=match(links2$Tax.Type, nodes2$name)-1
links2$IDtarget=match(links2$CTBA.Fund.Category, nodes2$name)-1
#colors assigned by node name into javascript - HEX CODES WORK
#color map
p4c <- data.frame(
  ttc= c(as.character(links2$Tax.Type), as.character(links2$CTBA.Fund.Category)),
  color= c(as.character(links2$TaxTypeColor), as.character(links2$FundCategoryColor)))
p4c <- unique(p4c)
#color coding
nodes2$group=as.character(1:length(nodes2$name))
test2 <- paste('"', p4c$color, '"', sep = "", collapse = ',')
nodelist2 <- paste('"', nodes2$group, '"', sep = "", collapse = ",") 
mycolor2 <- paste0('d3.scaleOrdinal() .domain([', nodelist2, ']) .range([', test2, '])')
#sankey continued
d <- sankeyNetwork(Links = links2, Nodes = nodes2,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Expected.Amount", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30, colourScale = noquote(mycolor2), NodeGroup = "group")
d

saveNetwork(d, "Product4.html")

#===================================================================================================
#PRODUCTS NUMBER 5-6 - EXPENDITURE INFORMATION
data2 <- read.xlsx("./2020/Totals.xlsx",10)

#condense entries
data2 <- data2 %>% group_by(CTBA.Fund.Category,Fund.Category,CTBA.Category) %>%
  summarize(`2020.ORDINANCE.(AMOUNT.$)` = sum(`2020.ORDINANCE.(AMOUNT.$)`))
#assign colors via fund category
data2 <- merge(data2,fcc,by.x = "Fund.Category", by.y = "fcc", all = TRUE)
colnames(data2)[5] <- "FundCategoryColor"
#assign color via CTBA Function
data2$CFcolor <- "#4f4f4f"
#create color map
p5ca <- data.frame(
  fcc= c(data2$Fund.Category),
  color= c(as.character(data2$FundCategoryColor))
)
p5ca <- unique(p5ca)
p5cb <- data.frame(
  fcc= c(data2$CTBA.Category),
  color= c(as.character(data2$CFcolor))
)
p5cb <- unique(p5cb)
p5c <- c(as.character(p5ca$color),as.character(p5cb$color))
#Product Number 5 - Fund Category into CTBA Function Categories - Sankey Diagram - Color Coded by Fund Category
links3=data2
nodes3=data.frame(name=c(as.character(links3$Fund.Category), as.character(links3$CTBA.Category)) %>% unique())
links3$IDsource=match(links3$Fund.Category, nodes3$name)-1
links3$IDtarget=match(links3$CTBA.Category, nodes3$name)-1
#color coding
nodes3$group=as.character(1:length(nodes3$name))
test3 <- paste('"', p5c, '"', sep = "", collapse = ',')
nodelist3 <- paste('"', nodes3$group, '"', sep = "", collapse = ",") 
mycolor3 <- paste0('d3.scaleOrdinal() .domain([', nodelist3, ']) .range([', test3, '])')
#sankey continued
e <- sankeyNetwork(Links = links3, Nodes = nodes3,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "2020.ORDINANCE.(AMOUNT.$)", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30,colourScale = noquote(mycolor3), NodeGroup = "group")
e
saveNetwork(e,"product5.html")

#Product Number 6 - CTBA Fund Category into CTBA Function Categories - Sankey Diagram - Color Coded by Fund Category
links4=data2
nodes4=data.frame(name=c(as.character(links4$CTBA.Fund.Category), as.character(links4$CTBA.Category)) %>% unique())
links4$IDsource=match(links4$CTBA.Fund.Category, nodes4$name)-1
links4$IDtarget=match(links4$CTBA.Category, nodes4$name)-1
#color map
p6ca <- data.frame(
  fcc= c(data2$CTBA.Fund.Category),
  color= c(as.character(data2$FundCategoryColor))
)
p6ca <- unique(p6ca)
p6cb <- data.frame(
  fcc= c(data2$CTBA.Category),
  color= c(as.character(data2$CFcolor))
)
p6cb <- unique(p6cb)
p6c <- c(as.character(p6ca$color),as.character(p6cb$color))
#color coding
nodes4$group=as.character(1:length(nodes4$name))
test4 <- paste('"', p6c, '"', sep = "", collapse = ',')
nodelist4 <- paste('"', nodes4$group, '"', sep = "", collapse = ",") 
mycolor4 <- paste0('d3.scaleOrdinal() .domain([', nodelist4, ']) .range([', test4, '])')
#sankey continued
f <- sankeyNetwork(Links = links4, Nodes = nodes4,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "2020.ORDINANCE.(AMOUNT.$)", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30,colourScale = noquote(mycolor4), NodeGroup = "group")
f
saveNetwork(f, "product6.html")

#===================================================================================================
#PRODUCT NUMBER 7 - Broad Full Sankey - Tax Type to Fund Category to CTBA Function Category - Color Coded by Tax Type & Fund Category
rm(list=ls())
data3 <- read.xlsx("./2020/Totals.xlsx",11)

#sankey begins
links5=data3
nodes5=data.frame(name=c(as.character(links5$Source), as.character(links5$Target)) %>% unique())
links5$IDsource=match(links5$Source, nodes5$name)-1
links5$IDtarget=match(links5$Target, nodes5$name)-1
#assign colors via source - FIX
p7c <- data.frame(
  name=c(as.character(nodes5$name)),
  color=c("#ff6255","#0590b8","#9366ff","#ffa755","#5bdbff","#ff1300","#ff7a00","#752df3","#ffa29b","#a2ebff","#1fc3f1","#ffffff","#ff8719","#bf9ff7",
          "#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f",
          "#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f")
)
#color coding
nodes5$group=as.character(1:length(nodes5$name))
test5 <- paste('"', p7c$color, '"', sep = "", collapse = ',')
nodelist5 <- paste('"', p7c$name, '"', sep = "", collapse = ",") 
mycolor5 <- paste0('d3.scaleOrdinal() .domain([', nodelist5, ']) .range([', test5, '])')
#sankey continued
g <- sankeyNetwork(Links = links5, Nodes = nodes5,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Amount", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30,colourScale = noquote(mycolor5), NodeGroup = "group")
g
saveNetwork(g, "Product7.html")

#PRODUCT NUMBER 8 - Condensed Broad Full Sankey - Tax Type to Fund Category to CTBA Function Category - Color Coded by Tax Type & Fund Category
#Condense data
data4 <- data3 %>% group_by(Source, Target) %>% 
  summarize(Amount = sum(Amount)) 
#Sankey setup
links6=data4
nodes6=data.frame(name=c(as.character(links6$Source), as.character(links6$Target)) %>% unique())
links6$IDsource=match(links6$Source, nodes6$name)-1
links6$IDtarget=match(links6$Target, nodes6$name)-1
#color coding
p8c <- data.frame(
  name=c(as.character(nodes6$name)),
  color=c("#752df3","#ff8719","#a2ebff","#ff7a00","#ffffff","#9366ff","#ffa755","#ff6255","#bf9ff7","#0590b8","#ff1300","#5bdbff","#ffa29b","#1fc3f1",
          "#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f",
          "#4f4f4f","#4f4f4f","#4f4f4f","#4f4f4f")
)
#color coding
nodes6$group=as.character(1:length(nodes6$name))
test6 <- paste('"', p8c$color, '"', sep = "", collapse = ',')
nodelist6 <- paste('"', p8c$name, '"', sep = "", collapse = ",") 
mycolor6 <- paste0('d3.scaleOrdinal() .domain([', nodelist6, ']) .range([', test6, '])')
#sankey continued
h <- sankeyNetwork(Links = links6, Nodes = nodes6,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Amount", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30,colourScale = noquote(mycolor6), NodeGroup = "group")
h
saveNetwork(h, "Product8.html")

#DO NOT USE - COLOR CODING IS OFF
#PRODUCT 9 - Detailed Full Sankey - CTBA Tax Type to CTBA Fund Category to CTBA Function Category
#load data
data5 <- read.xlsx("./2020/Totals.xlsx",12)
#begin sankey 
links7=data5
nodes7=data.frame(name=c(as.character(links7$Source), as.character(links7$Target)) %>% unique())
links7$IDsource=match(links7$Source, nodes7$name)-1
links7$IDtarget=match(links7$Target, nodes7$name)-1
#color coding - Manually done with Vlookup in Excel, too complex for me to do in R
write.xlsx(nodes7,"p9nodes.xlsx")
p9c <- read_xlsx("2018Master-Sankey.xlsx", sheet = 5)
#color coding
nodes7$group=as.character(1:length(nodes7$name))
test7 <- paste('"', p9c$color, '"', sep = "", collapse = ',')
nodelist7 <- paste('"', p9c$name, '"', sep = "", collapse = ",") 
mycolor7 <- paste0('d3.scaleOrdinal() .domain([', nodelist7, ']) .range([', test7, '])')
#sankey continued
i <- sankeyNetwork(Links = links7, Nodes = nodes7,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Amount", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30, height = 800, width = 1280, colourScale = noquote(mycolor7),NodeGroup = "group")
i
saveNetwork(i, "Product9.html")

#Product 10 - Condensed - Detailed Full Sankey - CTBA Tax Type to CTBA Fund Category to CTBA Function Category
data6 <- data5 %>% group_by(Source, Target) %>%
  summarize(Amount = sum(Amount))
#begin sankey
links8=data6
nodes8=data.frame(name=c(as.character(links8$Source), as.character(links8$Target)) %>% unique())
links8$IDsource=match(links8$Source, nodes8$name)-1
links8$IDtarget=match(links8$Target, nodes8$name)-1
#color coding - Manually done in Excel using Vlookup
write.xlsx(nodes8,"p10nodes.xlsx")
p10c <- read_xlsx("2018Master-Sankey.xlsx", sheet = 6)
#coding
nodes8$group=as.character(1:length(nodes8$name))
test8 <- paste('"', p10c$color, '"', sep = "", collapse = ',')
nodelist8 <- paste('"', p10c$name, '"', sep = "", collapse = ",") 
mycolor8 <- paste0('d3.scaleOrdinal() .domain([', nodelist8, ']) .range([', test8, '])')
#sankey continued
j <- sankeyNetwork(Links = links8, Nodes = nodes8,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Amount", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30, height = 800, width = 1280, colourScale = noquote(mycolor8),NodeGroup = "group")
j
saveNetwork(j, "Product10.html")

#PRODUCT 11 - Detailed Tax Type into CTBA Fund Category - Sankey Diagram - Color Coded by Tax Type & Fund Category
#load data
data7 <- read_xlsx("2018 Revenue Sources.xlsx")
#assign colors
ttc <- data.frame(
  taxtype = c("Service Revenue","Local Tax","Local Non Tax Revenue","Intergovernmental Revenue","Proceeds & Transfers In","Property Tax", "Grant"),
  color= c("#5bdbff", "#ff6255", "#ffa755", "#9e66ff", "#0590b8", "#FF1300","#ff7a00")
)
#essentially a vlookup
data7 <- merge(data7,ttc,by.x = "Tax Type", by.y = "taxtype", all = TRUE)

colnames(data7)[11] <- "TaxTypeColor"
#fund category colors
fcc <- data.frame(
  fcc = c("Corporate Funds","Enterprise Funds","Special Revenue Funds","Debt Service Funds","Pension Trust Funds", "TIF Admin Funds","Grant Funds"),
  color= c("#752df3", "#a2ebff", "#ffa29b", "#ff8719", "#bf9ff7", "#1fc3f1", "FFFFFF")
)

data7 <- merge(data7,fcc,by.x = "Fund Category", by.y = "fcc", all = TRUE)

colnames(data7)[12] <- "FundCategoryColor"
#condense entries
data7 <- data7 %>% group_by(`Fund Category`,`CTBA Category`, `Tax Type`, `CTBA Fund Category`, TaxTypeColor, FundCategoryColor) %>%
  summarize(`Expected Amount` = sum(`Expected Amount`))
#begin sankey
links9=data7
nodes9=data.frame(name=c(as.character(links9$`CTBA Category`), as.character(links9$`CTBA Fund Category`)) %>% unique())
links9$IDsource=match(links9$`CTBA Category`, nodes9$name)-1
links9$IDtarget=match(links9$`CTBA Fund Category`, nodes9$name)-1
#color map
p11c <- data.frame(
  name= c(as.character(links9$`CTBA Category`), as.character(links9$`CTBA Fund Category`)),
  color= c(as.character(links9$TaxTypeColor), as.character(links9$FundCategoryColor)))
p11c <- unique(p11c)
#colors coding
nodes9$group=as.character(1:length(nodes9$name))
test9 <- paste('"', p11c$color, '"', sep = "", collapse = ',')
nodelist9 <- paste('"', nodes9$group, '"', sep = "", collapse = ",") 
mycolor9 <- paste0('d3.scaleOrdinal() .domain([', nodelist9, ']) .range([', test9, '])')
#sankey continued
k <- sankeyNetwork(Links = links9, Nodes = nodes9,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Expected Amount", NodeID = "name",
                   sinksRight=FALSE, fontSize = 16, units = "$", fontFamily = "Helvetica",
                   nodePadding=10, nodeWidth = 30, colourScale = noquote(mycolor9), NodeGroup = "group")
k

saveNetwork(k, "Product11.html")
