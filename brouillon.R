
library(rgeolocate)


setwd("C:/Users/bapti/Onedrive/Bureau/Securite_challenge")
bdd = read.table("test.csv", sep=";", header = TRUE, encoding="utf-8")

print(head(bdd))
file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")


results <- maxmind("45.129.33.10" ,file, c("country_name"))
print(results)

test <- function(data){
  vect <- maxmind(data, file, c("country_name"))
}
bdd$country <- lapply(bdd$ipsrc, test)

colnames(bdd)[2] <- "id"


head(bdd)


print(head(bdd))

library(dplyr)

by_ipsrc <- bdd %>% group_by(ipsrc)
print(head(by_ipsrc))

library(sqldf)

analyse <- sqldf("select ipsrc, count(*) as nombre,
                 count(distinct ipdst) as cnbripdst, count(distinct dstport) as cnportdst,
                 sum(case when action like 'DENY' then 1 END) as deny,
                 sum(case when action like 'DENY' AND dstport < 1024 then 1 END) as inf1024deny,
                 sum(case when action like 'DENY' AND dstport >= 1024 then 1 END) as sup1024deny,
                 sum(case when action like 'DENY' AND (portdst = 21 OR portdst = 22 OR portdst = 3389 OR portdst = 3306) then 1 END) as admindeny)
                 from bdd group by ipsrc")

print(analyse)
