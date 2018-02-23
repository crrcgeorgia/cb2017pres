library(ggplot2)
library(extrafont)
library(tidyr)
library(plyr)
library(readstata13)
library(gridExtra)
library(ggalluvial)

setwd("D:\\Dropbox\\My projects\\CRRC\\Caucasus Barometer\\2017\\analysis\\model\\visuals")


theme_plot <- theme(
  axis.text.y = element_text(colour="black", size = 16, family = "Gill Sans MT"),
  axis.text.x = element_text(colour="black", size = 16, family="Gill Sans MT"),
  axis.title.x = element_text(size=16, family = "Futura Hv BT"),
  axis.title.y = element_text(size=16, family = "Futura Hv BT"),
  strip.text  = element_text(size=16, family = "Futura Hv BT"),
  panel.border = element_rect(fill=NA, linetype = "solid", colour = "black"),
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, colour = "Black", size=18, family = "Futura Hv BT"),
  plot.subtitle = element_text(hjust = 0.5, colour = "Black", size=14, family = "Futura Hv BT"),
  legend.title = element_text(size=12, family = "Futura Hv BT"),
  legend.text = element_text(size=14, family = "Gill Sans MT"),
  plot.caption = element_text(size=12, family = "Gill Sans MT")
)


#### slide1 

rand3 <- read.csv("../tables/rand3.csv", sep="\t")
names(rand3) <- c("Cat", "Prop", "LB", "UB")
rand3$Cat <- revalue(rand3$Cat, c("Have equally close relations with both Unions"="Have equally close relations\nwith both Unions",
"DK/RA"="(DK/RA)" ,"Join the EEU"="Join the Eurasian Economic Union,\nled by Russia",
"Not join any union"="(Not join any Union)",
"Join the EU"="Join the European Union,\nled by the Western European countries"))

rand3$Cat <- factor(rand3$Cat, levels=c("(DK/RA)", "(Not join any Union)",
			"Have equally close relations\nwith both Unions",  "Join the European Union,\nled by the Western European countries",
			"Join the Eurasian Economic Union,\nled by Russia"))

rand3p <- ggplot(rand3, aes(x=Cat, y=Prop, fill=Cat))+
	geom_bar(stat="identity", position="stack")+
	labs(title="In your opinion, what would be the best choice\nfor Georgia’s future economic and political development? (%)")+
	scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#8da0cb","#a6d854","#fc8d62", "#66c2a5"))+
	scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
	coord_flip(xlim=c(1, 5))+
	geom_text(data=rand3,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
	theme_plot+
	theme(
		plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank(),
		legend.position = "none",
		legend.direction = "vertical"
	)
				
ggsave("static/en/slide1.png", rand3p, height=7, width=13.5, dpi=300, device="png")


#### Slide 2

rand4 <- read.csv("../tables/rand4.csv", sep="\t")
names(rand4) <- c("Cat", "Prop", "LB", "UB")
rand4$Cat <- revalue(rand4$Cat, c("Have equally close relations with both organizations"="Have equally close relations\nwith both organizations",
"DK/RA"="(DK/RA)" ,"Join Collective Security Treaty Organization"="Join Collective Security Treaty Organization,\nled by Russia",
"Not join any organization "="(Not join any organization)",
"Join NATO "="Join NATO, led by the Western European countries\nand the US"))

rand4$Cat <- factor(rand4$Cat, levels=c("(DK/RA)", "(Not join any organization)",
			"Have equally close relations\nwith both organizations",  "Join NATO, led by the Western European countries\nand the US",
			"Join Collective Security Treaty Organization"="Join Collective Security Treaty Organization,\nled by Russia"))

rand4p <- ggplot(rand4, aes(x=Cat, y=Prop, fill=Cat))+
  geom_bar(stat="identity", position="stack")+
  labs(x = "Answer option",
       
       title="In your opinion, what would be the best choice for Georgia\nto ensure its security? (%)")+
 scale_fill_manual(name="Category",
  					values=c("#999999","#8da0cb","#a6d854","#fc8d62", "#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  coord_flip(xlim=c(1, 5))+
  geom_text(data=rand4,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
			
  theme_plot+
  theme(
  		plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank(),
		legend.position = "none",
		legend.direction = "vertical"
  )
				
ggsave("static/en/slide2.png", rand4p, height=7, width=13.5, dpi=300, device="png")



#### Slide 3

rand2 <- read.csv("../tables/rand2.csv", sep="\t")
names(rand2) <- c("Cat", "Prop", "LB", "UB")

rand2_dk<- rand2[1, ]
rand2_1 <- rand2[2:3, ]
rand2_2 <- rand2[4:5, ]
rand2_1$Strat <- c("Georgia's neutrality could help resolve conflicts\nand improve Georgia's security")
rand2_1$gr[rand2_1$Cat == "Somewhat agree: Neutrality could resolve conflicts"] <- c("Somewhat agree")
rand2_1$gr[rand2_1$Cat == "Strongly agree: Neutrality could resolve conflicts"] <- c("Strongly agree")

rand2_2$Strat <- c("Alignment with a certain bloc\nwould bring more benefits to Georgia")
rand2_2$gr[rand2_2$Cat == "Somewhat agree: Alignment with a block would bring more benefits"] <- c("Somewhat agree")
rand2_2$gr[rand2_2$Cat == "Strongly agree: Alignment with a block would bring more benefits"] <- c("Strongly agree")

rand2_dk$Strat <- c("(DK/RA)")
rand2_dk$gr <- c("(DK/RA)")
rand2_dk$Prop <- as.numeric(rand2_dk$Prop)
rand2_dk$LB <- as.numeric(rand2_dk$LB)
rand2_dk$UB <- as.numeric(rand2_dk$UB)

rand2_1$gr <- factor(rand2_1$gr, levels=c("Strongly agree", "Somewhat agree"))
rand2_2$gr <- factor(rand2_2$gr, levels=c("Strongly agree", "Somewhat agree"))


rand2p <- ggplot(rand2, aes(x=Strat, fill=gr, group=gr))+
  geom_bar(data=rand2_1, aes(y=-Prop), stat="identity", position="stack", width = 0.5)+
  geom_bar(data=rand2_2, aes(y=Prop), stat="identity", position="stack", width = 0.5)+
  geom_bar(data=rand2_dk, aes(y=Prop), stat="identity", position="stack", width = 0.5)+
scale_fill_manual(name="Options",
					labels=c("DK/RA", "Somewhat agree", "Strongly agree"),
  					values=c("#999999","#dfc27d", "#018571"))+
	geom_text(data=rand2_dk,
            aes(x=Strat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),              
			position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
  geom_text(data=rand2_1,
            aes(x=Strat,y=-Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
	geom_text(data=rand2_2,
            aes(x=Strat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),              
			position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
	labs(title="Which of these two statements do you agree with more? (%)")+
  guides(fill = guide_legend(reverse=T))+
  scale_y_continuous(labels=function(x)abs(x)*100, limits=c(-0.6, 0.6))+
  coord_flip()+
  theme_plot+
  theme(
    	plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_blank(),
		# axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		axis.ticks.x = element_blank(),
		legend.direction = "horizontal",
		legend.position = "bottom",
		legend.title = element_blank()
  )



ggsave("static/en/slide3.png", rand2p, height=7, width=13.5, dpi=300, device="png")
ggsave("static/en/slide3.svg", rand2p, height=7, width=13.5, dpi=300)

#### Slide 4:

nato <- read.csv("../tables/p22.csv", sep="\t")
eu <- read.csv("../tables/p25.csv", sep="\t")
eec <- read.csv("../tables/p28.csv", sep="\t")

nato$Strat <- c("NATO?")
eu$Strat <- c("The European Union?")
eec$Strat <- c("The Eurasian Economic Union?")
names(nato) <- c("Cat", "Prop", "LB", "UB", "Strat")
names(eu) <- c("Cat", "Prop", "LB", "UB", "Strat")
names(eec) <- c("Cat", "Prop", "LB", "UB", "Strat")
memb <- rbind(nato, eu, eec)
memb$Cat1 <- memb$Cat
memb$Cat1 <- factor(memb$Cat1, levels=c("DK/RA", "Do not support at all",
				"Rather not support", "Partially support, partially do not support",
				"Rather support", "Fully support"))
				
memb$Cat <- revalue(memb$Cat, c("Do not support at all"="Do not support at all",
"DK/RA"="(DK/RA)" ,"Fully support"="Fully support",
"Partially support, partially do not support"="Partially support, partially do not support",
"Rather not support"="Rather not support","Rather support"="Rather support"))

memb$Cat <- factor(memb$Cat, levels=c("(DK/RA)", "Do not support at all", "Rather not support",  "Partially support, partially do not support", "Rather support", "Fully support"))
memb$Strat <- factor(memb$Strat, levels=c("The Eurasian Economic Union?", 
                                          "The European Union?", 
                                          "NATO?"))

membp <- ggplot(memb, aes(x=Strat, fill=Cat))+
  geom_bar(data=memb, aes(y=Prop), stat="identity", position="stack")+
  labs(x = "Organization",
       
       title="To what extent would you support Georgia's membership in ...? (%)")+
 scale_fill_manual(name="Category",
  					values=c("#999999","#a6611a","#dfc27d","#d6d6d6","#80cdc1", "#018571"))+
  scale_y_continuous(labels=function(x)x*100)+
  guides(fill = guide_legend(reverse=T))+
  coord_flip(xlim=c(1, 3))+
  geom_text(data=memb,
            aes(x=Strat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT", color="white")+
  theme_plot+
  theme(
    	plot.title = element_text(hjust = 0.5),
		plot.subtitle = element_text(hjust = 0.5),
		axis.title.y = element_blank(),
		axis.title.x = element_blank(),
		axis.text.x = element_blank(),
		# axis.text.y = element_blank(),
		axis.ticks.y = element_blank(),
		axis.ticks.x = element_blank(),
		legend.direction = "horizontal",
		legend.position = "bottom",
		legend.text.align = 0,
		legend.title = element_blank(),
		plot.margin = unit(c(0.5,3.5,1,0.5), "cm")
	)

print(membp)				

ggsave("static/en/slide4.png", membp, height=7, width=13.5, dpi=300, device="png")
ggsave("static/en/slide4.svg", membp, height=7, width=13.5, dpi=300)



### Slide 5

nato9 <- read.csv("../tables/natosupr09.csv", sep="\t")
nato9$year <- 2009
names(nato9) <- c("Cat", "Prop", "LB", "UB", "Year")

nato10 <- read.csv("../tables/natosupr10.csv", sep="\t")
nato10$year <- 2010
names(nato10) <- c("Cat", "Prop", "LB", "UB", "Year")

nato11 <- read.csv("../tables/natosupr11.csv", sep="\t")
nato11$year <- 2011
names(nato11) <- c("Cat", "Prop", "LB", "UB", "Year")

nato12 <- read.csv("../tables/natosupr12.csv", sep="\t")
nato12$year <- 2012
names(nato12) <- c("Cat", "Prop", "LB", "UB", "Year")

nato13 <- read.csv("../tables/natosupr13.csv", sep="\t")
nato13$year <- 2013
names(nato13) <- c("Cat", "Prop", "LB", "UB", "Year")

nato15 <- read.csv("../tables/natosupr15.csv", sep="\t")
nato15$year <- 2015
names(nato15) <- c("Cat", "Prop", "LB", "UB", "Year")

nato17 <- read.csv("../tables/natosupr17.csv", sep="\t")
nato17$year <- 2017
names(nato17) <- c("Cat", "Prop", "LB", "UB", "Year")

nato <- as.data.frame(rbind(nato9, nato10, nato11, nato12, nato13, nato15, nato17))

nato$Cat <- revalue(nato$Cat, c("Do not support"="Do not support**", "In the middle"="Partially support, partially don’t support", "Support"="Support*", "DK/RA"="(DK/RA)"))

nato$Year <- factor(nato$Year, levels=c("2009", "2010", "2011",
                                        "2012", "2013", "2015", "2017"))
slide5 <- ggplot(nato, aes(Year, Prop, group=Cat, label=Prop))+
  geom_line(aes(colour=Cat))+
  geom_point(aes(shape=Cat, colour=Cat), size=2)+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  geom_ribbon(aes(fill=Cat, ymin=LB, ymax=UB),
              alpha=0.2)+
  scale_color_manual(values=c("#999999", "#a6611a",  "#67a9cf", "#018571"))+
  scale_fill_manual(guide=FALSE, values=c("#999999", "#a6611a",  "#67a9cf", "#018571"))+
  scale_shape(guide="none")+
  guides(color = guide_legend(reverse=T))+
  scale_y_continuous(breaks=c(0.25, 0.50, 0.75, 1), labels=function(x)x*100)+
  labs(title="Georgia's membership in NATO: (%)",
		caption="\n* ''Fully support'' + ''Rather support''\n**''Don’t support at all'' + ''Rather not support''")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
	plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.direction = "vertical",
	  legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
	plot.caption = element_text(size=12, family = "Gill Sans MT"),
	  plot.margin = unit(c(0.5,2,1,0.5), "cm")
  )

print(slide5)

ggsave("static/en/slide5.png", slide5, height=7, width=13.5, dpi=300, device="png")

#### Slide 6

eu11 <- read.csv("../tables/eusupr11.csv", sep="\t")
eu11$year <- 2011
names(eu11) <- c("Cat", "Prop", "LB", "UB", "Year")

eu12 <- read.csv("../tables/eusupr12.csv", sep="\t")
eu12$year <- 2012
names(eu12) <- c("Cat", "Prop", "LB", "UB", "Year")

eu13 <- read.csv("../tables/eusupr13.csv", sep="\t")
eu13$year <- 2013
names(eu13) <- c("Cat", "Prop", "LB", "UB", "Year")

eu15 <- read.csv("../tables/eusupr15.csv", sep="\t")
eu15$year <- 2015
names(eu15) <- c("Cat", "Prop", "LB", "UB", "Year")

eu17 <- read.csv("../tables/eusupr17.csv", sep="\t")
eu17$year <- 2017
names(eu17) <- c("Cat", "Prop", "LB", "UB", "Year")

eu <- as.data.frame(rbind(eu11, eu12, eu13, eu15, eu17))
eu <- subset(eu, !Cat %in% c("Don't know", "Refuse to answer"))

eu$Cat <- revalue(eu$Cat, c("Do not support"="Do not support**", "In the middle"="Partially support, partially don’t support", "Support"="Support*", "DK/RA"="(DK/RA)"))

eu$Year <- factor(eu$Year, levels=c("2009", "2010", "2011",
                                        "2012", "2013", "2015", "2017"))
										
slide6 <- ggplot(eu, aes(Year, Prop, group=Cat, label=Prop))+
  geom_line(aes(colour=Cat))+
  geom_point(aes(shape=Cat, colour=Cat), size=2)+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  geom_ribbon(aes(fill=Cat, ymin=LB, ymax=UB),
              alpha=0.2)+
  scale_color_manual(values=c("#999999", "#a6611a",  "#67a9cf", "#018571"))+
  scale_fill_manual(guide=FALSE, values=c("#999999", "#a6611a",  "#67a9cf", "#018571"))+
  scale_y_continuous(breaks=c(0.25, 0.50, 0.75, 1), labels=function(x)x*100)+
    scale_shape(guide="none")+
  guides(color = guide_legend(reverse=T))+
  labs(title="Georgia's membership in the European Union: (%)",
		caption="\n* ''Fully support'' + ''Rather support''\n**''Don’t support at all'' + ''Rather not support''")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
	plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.direction = "vertical",
	  legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
	plot.caption = element_text(size=12, family = "Gill Sans MT"),
	  plot.margin = unit(c(0.5,2,1,0.5), "cm")
  )
  
 
print(slide6)

ggsave("static/en/slide6.png", slide6, height=7, width=13.5, dpi=300, device="png")

### Slide 7

eec13 <- read.csv("../tables/eecsupr13.csv", sep="\t")
eec13$year <- 2013
names(eec13) <- c("Cat", "Prop", "LB", "UB", "Year")

eec15 <- read.csv("../tables/eecsupr15.csv", sep="\t")
eec15$year <- 2015
names(eec15) <- c("Cat", "Prop", "LB", "UB", "Year")

eec17 <- read.csv("../tables/eecsupr17.csv", sep="\t")
eec17$year <- 2017
names(eec17) <- c("Cat", "Prop", "LB", "UB", "Year")

eec <- as.data.frame(rbind(eec13, eec15, eec17))

eec <- subset(eec, !Cat %in% c("Don't know", "Refuse to answer"))

eec$Cat <- revalue(eec$Cat, c("Do not support"="Do not support**", "In the middle"="Partially support, partially don’t support", "Support"="Support*", "DK/RA"="(DK/RA)"))

eec$Year <- factor(eec$Year, levels=c("2009", "2010", "2011",
                                        "2012", "2013", "2015", "2017"))

eec$Year <- revalue(eec$Year, c("2017"="2017***"))
	
slide7 <- ggplot(eec, aes(Year, Prop, group=Cat, label=Prop))+
  geom_line(aes(colour=Cat))+
  geom_point(aes(shape=Cat, colour=Cat), size=2)+
  coord_cartesian(ylim = c(0, 1), expand = FALSE)+
  geom_ribbon(aes(fill=Cat, ymin=LB, ymax=UB),
              alpha=0.2)+
  scale_color_manual(values=c("#999999", "#a6611a",  "#67a9cf", "#018571"))+
  scale_fill_manual(guide=FALSE, values=c("#999999", "#a6611a",  "#67a9cf", "#018571"))+
  scale_y_continuous(breaks=c(0.25, 0.50, 0.75, 1), labels=function(x)x*100)+
    scale_shape(guide="none")+
	guides(color = guide_legend(reverse=T))+
  labs(title="Georgia's membership in the Eurasian Economic Union,\nled by the Russian Federation: (%)",
		caption="\n* ''Fully support'' + ''Rather support''\n**''Don’t support at all'' + ''Rather not support''\n***The wording that the EEU is led by the Russian Federation was added in 2017")+
  theme_plot+
  theme(
    plot.title = element_text(hjust = 0.5),
	plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.direction = "vertical",
	  legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
	plot.caption = element_text(size=12, family = "Gill Sans MT"),
	  plot.margin = unit(c(0.5,2,1,0.5), "cm")
  )
  

print(slide7)

ggsave("static/en/slide7.png", slide7, height=7, width=13.5, dpi=300, device="png")



#### Alluvial diagrama


alv <- read.csv("alluv.csv", sep="\t", encoding = "UTF-8")
is_alluvial(as.data.frame(alv), logical = FALSE, silent = TRUE)

alv$p25 <- revalue(alv$p25, c("Do not support"="Do not support", "Neutral"="Partially support,\npartially don’t support", "Support"="Support", "DK/RA"="(DK/RA)"))

alv$p25 <- factor(alv$p25, levels=c("Support", "Partially support,\npartially don’t support", "Do not support",  "(DK/RA)"))

alv$p28 <- revalue(alv$p28, c("Do not support"="Do not support", "Neutral"="Partially support,\npartially don’t support", "Support"="Support", "DK/RA"="(DK/RA)"))

alv$p28 <- factor(alv$p28, levels=c("Support", "Partially support,\npartially don’t support", "Do not support",  "(DK/RA)"))

alvp <- ggplot(alv,
       aes(weight = Prop, axis1 = p25, axis2 = p28)) +
  geom_alluvium(aes(fill=p25), width = 1/8) +
  geom_stratum(width = 1/4, fill = "white", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE, 
             label.size=NA, family="Gill Sans MT", size=4) +
  scale_x_continuous(breaks = 1:2, labels = c("", ""), position = "bottom")+
  guides(fill = FALSE) +
  theme_plot+
  theme(
    # axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
	  axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size=14, face = "bold", color="black",
                               family="Futura Hv BT")
    )+
  scale_fill_manual(values=c("#018571", "#4daf4a", "#a6611a", "#999999"))

ggsave("static/en/alluvial.png", alvp, height=7, width=13.5, dpi=300, device="png")
ggsave("static/en/alluvial.svg", alvp, height=7, width=13.5)


##### Slide 11

p24 <- read.csv("../tables/p24.csv", sep="\t")

names(p24) <- c("Cat", "Prop", "LB", "UB")

p241 <- subset(p24, !Cat %in% c("DK/RA", "Other"))

p241$Strat <- c("Question")
p241$Strat[p241$Cat=="DK/RA"] <- c("DK/RA")
p241$Cat <- factor(p241$Cat, levels = p241$Cat[order(p241$Prop)])

p24dkra <- subset(p24, Cat %in% c("DK/RA", "Other"))
p24dkra$Cat <- factor(p24dkra$Cat, levels = c("DK/RA", "Other"))
p24dkra$Strat <- c("Question")
p24dkra$Strat[p24dkra$Cat=="DK/RA"] <- c("DK/RA")

p241 <- rbind(p24dkra, p241)

p241$Cat <- revalue(p241$Cat, c("DK/RA"="(DK/RA)",
"Other"="Other",
"It would result in improved safety in everyday life "="It would result in improved safety\nin everyday life",
"It would help strengthen our relations with the West"="It would help strengthen\nour relations with the West",
"Peoples economic conditions would improve"="People's economic conditions\nwould improve",
"My country would have a better chance to restore its territorial integrity"="Georgia would have a better chance\nto restore its territorial integrity",
"We would be better protected from foreign threats"="We would be better protected\nfrom foreign threats"
))

slide11 <- ggplot(p241, aes(y=Prop, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Option",
       
       title="What is the main reason you would support Georgia's membership in NATO? (%)",
	   subtitle="The question was asked to the 41% who supported Georgia's membership in NATO")+
  geom_text(data=p241,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("static/en/slide11.png", slide11, height=7, width=13.5, dpi=300, device="png")


##### Slide 12
p27 <- read.csv("../tables/p27.csv", sep="\t")

names(p27) <- c("Cat", "Prop", "LB", "UB")

p271 <- subset(p27, !Cat %in% c("DK/RA", "Other"))

p271$Strat <- c("Question")
p271$Strat[p271$Cat=="DK/RA"] <- c("DK/RA")
p271$Cat <- factor(p271$Cat, levels = p271$Cat[order(p271$Prop)])

p27dkra <- subset(p27, Cat %in% c("DK/RA", "Other"))
p27dkra$Cat <- factor(p27dkra$Cat, levels = c("DK/RA", "Other"))
p27dkra$Strat <- c("Question")
p27dkra$Strat[p27dkra$Cat=="DK/RA"] <- c("DK/RA")

p271 <- rbind(p27dkra, p271)

p271$Cat <- revalue(p271$Cat, c("DK/RA"="(DK/RA)",
"Other"="Other",
"EU countries would get to know better our culture and traditions"="EU countries would get to know better\nour culture and traditions",
"I would be able to travel to the EU countries without a visa"="I would be able to travel\nto the EU countries without a visa",
"It would result in improved safety in everyday life "="It would result in\nimproved safety in everyday life",
"We would be better protected from foreign threats"="We would be better protected\nfrom foreign threats",
"My country would have a better chance to restore its territorial integrity"="Georgia would have a better chance\nto restore its territorial integrity",
"It would help strengthen our relations with the West"="It would help strengthen\nGeorgia's relations with the West",
"Peoples economic conditions would improve"="People's economic conditions would improve"
))

slide12 <- ggplot(p271, aes(y=Prop, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Option",
       
       title="What is the main reason you would support\nGeorgia’s membership in the EU? (%)",
	   subtitle="The question was asked to the 46%\nwho supported Georgia's membership in the EU")+
  geom_text(data=p271,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("static/en/slide12.png", slide12, height=7, width=13.5, dpi=300, device="png")

##### Frequency charts p30
p30 <- read.csv("../tables/p30.csv", sep="\t")

names(p30) <- c("Cat", "Prop", "LB", "UB")

p301 <- subset(p30, !Cat %in% c("DK/RA", "Other"))

p301$Strat <- c("Question")
p301$Strat[p301$Cat=="DK/RA" ] <- c("DK/RA")
p301$Cat <- factor(p301$Cat, levels = p301$Cat[order(p301$Prop)])

p30dkra <- subset(p30, Cat %in% c("DK/RA", "Other"))
p30dkra$Cat <- factor(p30dkra$Cat, levels = c("DK/RA", "Other"))
p30dkra$Strat <- c("Question")
p30dkra$Strat[p30dkra$Cat=="DK/RA"] <- c("DK/RA")

p301 <- rbind(p30dkra, p301)

p301$Cat <- revalue(p301$Cat, c("DK/RA"="(DK/RA)",
"Other"="Other",
"I would be able to travel to the EEU countries without a visa"="I would be able to travel\nto the EEU countries without a visa",
"It would result in improved safety in everyday life "="It would result in improved safety\nin everyday life",
"We would be better protected from foreign threats"="We would be better protected\nfrom foreign threats",
"My country would have a better chance to restore its territorial integrity"="My country would have a better chance\nto restore its territorial integrity",
"It would help strengthen our relations with Russia"="It would help strengthen\nour relations with Russia",
"Peoples economic conditions would improve"="People's economic conditions would improve"
))

slide13 <- ggplot(p301, aes(y=Prop, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Option",
       
       title="What is the main reason you would support\nGeorgia’s membership in the EEU? (%)",
	   subtitle="The question was asked to the 19%\nwho supported Georgia's membership in the EEU")+
  geom_text(data=p301,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("static/en/slide13.png", slide13, height=7, width=13.5, dpi=300, device="png")

##### slide 14
p31 <- read.csv("../tables/p31.csv", sep="\t")

names(p31) <- c("Cat", "Prop", "LB", "UB")

p311 <- subset(p31, !Cat %in% c("Refuse to answer", "Don't know", "None"))

p311$Strat <- c("Question")
p311$Strat[p311$Cat=="Don't know" | p311$Cat=="Refuse to answer"] <- c("DK/RA")
p314 <- subset(p311, Prop < 0.05)
p311 <- subset(p311, Prop >= 0.05)
p313 <- as.data.frame(cbind("Other", sum(p314$Prop), mean(p314$LB), mean(p314$UB), "Question"), stringsAsFactors = FALSE)
names(p313) <- c("Cat", "Prop", "LB", "UB", "Strat")
p313$Prop <- as.numeric(p313$Prop)
p313$LB <- as.numeric(p313$LB)
p313$UB <- as.numeric(p313$UB)
p311$Cat <- factor(p311$Cat, levels = p311$Cat[order(p311$Prop)])

p31dkra <- subset(p31, Cat %in% c("Refuse to answer", "Don't know", "None"))
p31dkra$Strat <- c("Question")
p31dkra <- rbind(p313, p31dkra)
p31dkra$Cat <- factor(p31dkra$Cat, levels = c("Refuse to answer", "Don't know", "None", "Other"))
p31dkra$Strat[p31dkra$Cat=="Don't know" | p31dkra$Cat=="Refuse to answer"] <- c("DK/RA")


p311 <- rbind(p31dkra, p311)

p311$Cat <- revalue(p311$Cat, c("Don't know"="(Don't know)",
"Refuse to answer"="(Refuse to answer)",
"Other"="Other",
"None"="(None)",
"Russia"="Russia",
"Turkey"="Turkey",
"Ukraine"="Ukraine",
"Azerbaijan"="Azerbaijan",
"USA"="United States"
))

slide14 <- ggplot(p311, aes(y=Prop, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Country",
       
       title="In your opinion, which country is currently the main friend of Georgia? (%)")+
  geom_text(data=p311,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#66c2a5"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("static/en/slide14.png", slide14, height=7, width=13.5, dpi=300, device="png")

##### slide 15
p32 <- read.csv("../tables/p32.csv", sep="\t")

names(p32) <- c("Cat", "Prop", "LB", "UB")

p321 <- subset(p32, !Cat %in% c("Refuse to answer", "Don't know", "None"))

p321$Strat <- c("Question")
p321$Strat[p321$Cat=="Don't know" | p321$Cat=="Refuse to answer"] <- c("DK/RA")
p324 <- subset(p321, Prop < 0.03)
p321 <- subset(p321, Prop >= 0.03)
p323 <- as.data.frame(cbind("Other", sum(p324$Prop), mean(p324$LB), mean(p324$UB), "Question"), stringsAsFactors = FALSE)
names(p323) <- c("Cat", "Prop", "LB", "UB", "Strat")
p323$Prop <- as.numeric(p323$Prop)
p323$LB <- as.numeric(p323$LB)
p323$UB <- as.numeric(p323$UB)
p321$Cat <- factor(p321$Cat, levels = p321$Cat[order(p321$Prop)])

p32dkra <- subset(p32, Cat %in% c("Refuse to answer", "Don't know", "None"))
p32dkra$Strat <- c("Question")
p32dkra <- rbind(p323, p32dkra)
p32dkra$Cat <- factor(p32dkra$Cat, levels = c("Refuse to answer", "Don't know", "None", "Other"))
p32dkra$Strat[p32dkra$Cat=="Don't know" | p32dkra$Cat=="Refuse to answer"] <- c("DK/RA")


p321 <- rbind(p32dkra, p321)

p321$Cat <- revalue(p321$Cat, c("Don't know"="(Don't know)",
"Refuse to answer"="(Refuse to answer)",
"Other"="Other",
"None"="(None)",
"Russia"="Russia",
"Turkey"="Turkey"
))

slide15 <- ggplot(p321, aes(y=Prop, x=Cat,group=Strat,fill=Strat))+
  geom_col()+
  coord_flip()+
  theme_plot+
  labs(x = "Country",
       
       title="In your opinion, which country is currently the main enemy of Georgia? (%)")+
  geom_text(data=p321,
            aes(x=Cat,y=Prop,label=ifelse(Prop > 0.015, sprintf("%0.f", round(Prop*100, digits = 0)), "")),
            position = position_stack(vjust=0.5),
			color="white",
            family="Futura Hv BT")+
 scale_fill_manual(name="კატეგორია",
  					values=c("#999999","#a6611a"))+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
		theme(legend.position = "none",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	axis.text.x = element_blank(),
	axis.ticks.x = element_blank(),
				legend.direction = "vertical")

ggsave("static/en/slide15.png", slide15, height=7, width=13.5, dpi=300, device="png")

##### Predicted Probabilities
########## Sex

margins1 <- read.dta13("../margins/p22r_sex.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at7", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_sex.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at7", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_sex.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at7", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("Male"="Male", "Female"="Female"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))


ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	legend.direction="horizontal",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank())



ggsave("static/en/slide16.png", height=7, width=13.5, dpi=300, device="png")

########## Age group

margins1 <- read.dta13("../margins/p22r_agegroup.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at5", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
# margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_agegroup.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at5", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
# margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_agegroup.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at5", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
# margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")

ggplot(models, aes(x=at, y=margin))+
  geom_line(aes(color=predict, group=predict))+
  scale_colour_manual(guide=FALSE, values=c("#e41a1c", "#377eb8"))+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"))+
  geom_ribbon(aes(fill=predict, ymax = upper, ymin = lower), alpha=0.2)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape(guide="none")+
  labs(title="Age")+
  facet_grid(~model)+
   guides(fill = guide_legend(reverse=T))+
    theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )
  

  
ggsave("static/en/slide17.png", height=7, width=13.5, dpi=300, device="png")

########## Settlement type

margins1 <- read.dta13("../margins/p22r_settype.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at6", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_settype.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at6", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_settype.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at6", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("1"="Tbilisi", "2"="Other\nurban", "3"="Rural"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )

ggsave("static/en/slide18.png", height=7, width=13.5, dpi=300, device="png")

########## Ethnicity

margins1 <- read.dta13("../margins/p22r_ethn.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at8", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_ethn.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at8", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_ethn.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at8", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("Georgian"="Georgian", "Other"="Other"))
models$at <- factor(models$at, levels=c("Georgian"="Georgian", "Other"="Other"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )

ggsave("static/en/slide19.png", height=7, width=13.5, dpi=300, device="png")



########## Education

margins1 <- read.dta13("../margins/p22r_edu.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at1", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_edu.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at1", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_edu.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at1", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")

models$at <- revalue(models$at, c("Secondary or lower"="Secondary\nor lower", "Vocational"="Secondary\ntechnical", "Higher"="Higher"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))
ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="Education")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )

ggsave("static/en/slide20.png", height=7, width=13.5, dpi=300, device="png")


########## Parents Education

margins1 <- read.dta13("../margins/p22r_parhedu.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at2", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_parhedu.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at2", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_parhedu.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at2", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")

models$at <- revalue(models$at, c("0"="No", "1"="Yes"))
models$at <- factor(models$at, levels=c("Yes", "No"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))
ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="Did any of the parents obtain higher education?")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )

ggsave("static/en/slide21.png", height=7, width=13.5, dpi=300, device="png")


########## Household income

margins1 <- read.dta13("../margins/p22r_hhincome.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at4", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_hhincome.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at4", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_hhincome.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at4", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")

models$at <- revalue(models$at, c("$101-250"="$101-\n250", "$251-400"="$251-\n400", "DK/RA"="DK/RA"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))
ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="Household income last month")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	axis.text.x = element_text(size=10),
	strip.background = element_blank()
    )

ggsave("static/en/slide22.png", height=7, width=13.5, dpi=300, device="png")


########## internet

margins1 <- read.dta13("../margins/p22r_internet.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_internet.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_internet.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("0"="No", "1"="Yes"))
models$at <- factor(models$at, levels=c("Yes", "No"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="Internet used at least once a month")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
#	axis.text.x = element_text(size=10),
	strip.background = element_blank()
    )

ggsave("static/en/slide23.png", height=7, width=13.5, dpi=300, device="png")

########## d13

margins1 <- read.dta13("../margins/p22r_d13r.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_d13r.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_d13r.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("No"="No", "Yes"="Yes"))
models$at <- factor(models$at, levels=c("Yes", "No"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="Attendance of religious services at least once a month")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )

ggsave("static/en/slide24.png", height=7, width=13.5, dpi=300, device="png")




########## m1

margins1 <- read.dta13("../margins/p22r_m1.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_m1.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_m1.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("Bad"="Bad",  "Neutral"="Neutral",  "Good_(3)"="Good"))
models$at <- factor(models$at, c("Good", "Neutral", "Bad"))

models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))


ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
	labs(title="Attitudes towards immigrants")+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	strip.background = element_blank()
    )

ggsave("static/en/slide25.png", height=7, width=13.5, dpi=300, device="png")

########## m2

margins1 <- read.dta13("../margins/p22r_m2.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_m2.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_m2.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("No"="No", "Yes"="Yes"))
models$at <- factor(models$at, levels=c("Yes", "No"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
	labs(title="Have you had any contact with foreigners in Georgia\nwho have stayed here for longer than 3 months?")+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
#	axis.text.x = element_text(size=10),
	strip.background = element_blank()
    )

ggsave("static/en/slide26.png", height=7, width=13.5, dpi=300, device="png")

########## p1

margins1 <- read.dta13("../margins/p22r_p1.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_p1.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_p1.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("Wrong"="Wrong", "Does not change at all"="No change", "Right"="Right"))
models$at <- factor(models$at, levels=c("Right"="Right", "No change"="No change", "Wrong"="Wrong"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
  labs(title="Direction of Georgia's domestic politics")+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
	# axis.text.x = element_text(size=12),
	strip.background = element_blank()
    )

ggsave("static/en/slide27.png", height=7, width=13.5, dpi=300, device="png")



########## p2

margins1 <- read.dta13("../margins/p22r_p2.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_p2.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_p2.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")


models$at <- revalue(models$at, c("Situation will never improve"="Situation\nwill never\nimprove", "Everything will be fine"="Everything\nwill be\nfine", "Agree with neither_(3)"="None"))

models <- subset(models, !at %in% "None")
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
  labs(title="Hope for future")+  
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
#	axis.text.x = element_text(size=14),
	strip.background = element_blank()
    )

ggsave("static/en/slide28.png", height=7, width=13.5, dpi=300, device="png")


########## p12

margins1 <- read.dta13("../margins/p22r_p12.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_p12.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)
margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_p12.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")

models$at <- revalue(models$at, c("Disagree"="Disagree", "Agree"="Agree", "Neither"="Neither"))
models$at <- factor(models$at, levels=c("Agree", "Disagree", "Neither"))
models <- subset(models, !at %in% "Neither")
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))


ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="People like yourself are treated fairly by the government?")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
# axis.text.x = element_text(size=8),
	strip.background = element_blank()
    )

ggsave("static/en/slide29.png", height=7, width=13.5, dpi=300, device="png")

########## p13

margins1 <- read.dta13("../margins/p22r_p13.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_p13.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)
margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_p13.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")

models$at <- revalue(models$at, c("People are like children"="Government should\ntake care of people", "Government is an employee"="People should\ncontrol\nthe government", "Agree with neither_(3)"="None"))
models <- subset(models, !at %in% "None")
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  facet_grid(~model)+
  labs(title="People and the government")+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
 axis.text.x = element_text(size=12),
	strip.background = element_blank()
    )

ggsave("static/en/slide30.png", height=7, width=13.5, dpi=300, device="png")

########## p17

margins1 <- read.dta13("../margins/p22r_p17.dta")
margins1 <- subset(margins1, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins1) <- c("predict", "margin", "at", "lower", "upper")
margins1$predict <- as.factor(margins1$predict)
margins1$at <- as.factor(margins1$at)

margins1$predict <- revalue(margins1$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins1$model <- c("NATO")

margins2 <- read.dta13("../margins/p25r_p17.dta")
margins2 <- subset(margins2, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins2) <- c("predict", "margin", "at", "lower", "upper")
margins2$predict <- as.factor(margins2$predict)
margins2$at <- as.factor(margins2$at)

margins2$predict <- revalue(margins2$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins2$model <- c("The European Union")

margins3 <- read.dta13("../margins/p28r_p17.dta")
margins3 <- subset(margins3, select=c("_predict", "_margin", "_at9", "_ci_lb", "_ci_ub"))
names(margins3) <- c("predict", "margin", "at", "lower", "upper")
margins3$predict <- as.factor(margins3$predict)
margins3$at <- as.factor(margins3$at)

margins3$predict <- revalue(margins3$predict, c("predict(pr outcome(3))"="Support", "predict(pr outcome(2))"="Neutral", "predict(pr outcome(1))"="Do not support"))
margins3$model <- c("The Eurasian Economic Union")

models <- as.data.frame(rbind(margins1, margins2, margins3))
models$model <- factor(models$model, levels=c("NATO", "The European Union", "The Eurasian Economic Union"))
models <- subset(models, !predict %in% "Neutral")
models$at <- revalue(models$at, c("No"="No", "Yes"="Yes"))
models$at <- factor(models$at, levels=c("Yes", "No"))
models$predict <- factor(models$predict, c("Support", "Neutral", "Do not support"))

ggplot(models, aes(x=at, y=margin, group=model, 
                   shape=predict, colour = predict))+
  geom_point(size=5)+
  scale_y_continuous(labels=function(x)x*100, limits=c(0, 1))+
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  geom_errorbar(aes(ymax = upper, ymin = lower), width=0.1, size=0.4)+
  scale_fill_manual(values=c("#377eb8", "#e41a1c", "#4daf4a"))+
  guides(fill = guide_legend(reverse=T))+
  labs(title="Did you vote in the Parliamentary elections on October 8, 2016?")+
  facet_grid(~model)+
  theme_plot+
  theme(
    legend.position="top",
	axis.title.y = element_blank(),
	axis.title.x = element_blank(),
	panel.grid.major.y = element_line(colour = "grey80"),
    legend.title = element_blank(),
#	axis.text.x = element_text(size=10),
	strip.background = element_blank()
    )

ggsave("static/en/slide31.png", height=7, width=13.5, dpi=300, device="png")


