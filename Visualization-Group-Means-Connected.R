# Group differences connected by lines
library(ggplot2)
library(ggalt) # devtools::install_github("hrbrmstr/ggalt")
library(dplyr)

# from: https://plot.ly/r/dumbbell-plots/
URL <- "https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil)

df <- read.csv(fil, stringsAsFactors=FALSE)
df <- arrange(df, desc(Men))
df <- mutate(df, School=factor(School, levels=rev(School)))

gg <- ggplot(df, aes(x=Women, xend=Men, y=School))
gg <- gg + geom_dumbbell(colour="#686868",
                         point.colour.l="#ffc0cb",
                         point.colour.r="#0000ff",
                         point.size.l=2.5,
                         point.size.r=2.5)
gg <- gg + scale_x_continuous(breaks=seq(60, 160, by=20),
                              labels=sprintf("$%sK", comma(seq(60, 160, by=20))))
gg <- gg + labs(x="Annual Salary", y=NULL,
                title="Gender Earnings Disparity",
                caption="Data from plotly")
gg <- gg + theme_bw()
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.title.x=element_text(hjust=1, face="italic", margin=margin(t=-24)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=24)))



