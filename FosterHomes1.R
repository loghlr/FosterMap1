# abb 12/28/2018: Tally foster home boarding county/zip by removal county (not zip)
# abb 1/15/2019: alter coords of small ZCTAs to big ones nearby
#source('FosterHomes1.R',echo=T,max.deparse.length=Inf)

stabb='GA'
Sys.umask('0133') # -rw-r--r--
#a1 <- read.table( '/fci/reports/ga/data/geocodes/recd20171208/afcars addresses 2017dec08.dsv.xz', header=T, sep='|', as.is=T, comment.char='', quote='"' )
a1 <- read.table( '/fci/reports/ga/data/geocodes/recd20190104a/afcars addresses, run 2019jan03.dsv.xz', header=T, sep='|', as.is=T, comment.char='', quote='"' )
str( a1 ) # 75998 obs. of  16 variables
# mostly zip+4, but zip5 less identifiable and probably sufficient.

a1$remzip5 <- sub( '^([0-9]{5}).*$', '\\1', a1$REMOVAL_ZIP )
a1$plczip5 <- sub( '^([0-9]{5}).*$', '\\1', a1$PLCMT_ZIP )
tail(sort(table(a1$remzip5)),20)

# convert county names to fips.
str(c1 <- read.csv( '/fci/src/uscounties.txt', as.is=T ))
c1$fips <- paste0( sprintf('%02d',as.numeric(c1[,2])), sprintf('%03d',as.numeric(c1[,3])) ) 
c1$CountyName <- paste( c1[,1], toupper(sub( ' +(parish|county|borough)$', '', c1$County.Name, ignore.case=T )) )
(stcofips <- c1$fips[c1[,1] == stabb & c1[,3] > 0])
sort(table(cn1 <- paste( stabb, toupper(sub( ' +(parish|county|borough)$', '', a1$REMOVAL_COUNTY, ignore.case=T ))) ))
table(!is.na(l11 <- match( cn1, c1$CountyName ))) # 1233 (1.8%) don't match
table( cn1[is.na(l11)], useNA='always' ) # all blank or out of state
stopifnot( cn1[is.na(l11)] %in% c('GA ', 'GA OUT OF STATE', 'GA -NONE-') )
table( a1$remcofips <- c1$fips[l11], useNA='always' )

sort(table(cn1 <- paste( stabb, toupper(sub( ' +(parish|county|borough)$', '', a1$PLACEMENT_COUNTY, ignore.case=T ))) ))
table(!is.na(l11 <- match( cn1, c1$CountyName ))) # 6607 (9.6%) don't match
table( cn1[is.na(l11)], useNA='always' ) # all none, na or out of state
stopifnot( cn1[is.na(l11)] %in% c('GA ', 'GA OUT OF STATE', 'GA -NONE-') )
table( a1$plccofips <- c1$fips[l11], useNA='always' )

# convert zips to zcta centroids.
tail(sort(table( a1$remzip5, useNA='always' )))
table( nchar(as.character(a1$remzip5)), useNA='always' )
#30060 30349 30701 30161       30032 
#  705   750   784   798   829   969 
#    0     5  <NA> 
#  829 75169     0

# zcta centroids?
#http://download.geonames.org/export/zip/ # unclear provenance, but updated today? not sure how they were calculated, hoping center of mass of population or some such?
str(zc1 <- read.table( '/fci/reports/us/census/geonameszip_US.txt.xz', sep='\t', header=F, as.is=T, colClasses='character' ))
colnames(zc1) <- unlist(strsplit('co zip zipname statename statefips countyname countyfips communityname communityfips lat lon accuracy',' '))
# ZCTA demographics, processed by me from ACS2017 5-year files:
str(zp1 <- read.csv( '/fci/reports/us/data/census/ACS2017/ACS5US860_2017a.csv' ))

table(!is.na(l11 <- match( sprintf('%05d',zp1[,1]), zc1[,2] ))) # 1081 (3.3%) don't match
summary( zp1$hh[!is.na(l11)] )
summary( zp1$hh[is.na(l11)] )
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     0     271    1064    3593    4945   42546 
#     0     266     794    3866    5036   36818
# close enough to MAR to ignore NA.
# To avoid identifiability, use alternate centroid in ZCTAs with < 10 households.
# if count of households (zp1$hh[l11]) < 10, set centroid to the nearest centroid with hh >= 10:
zc1$lat <- as.numeric(zc1$lat)
zc1$lon <- as.numeric(zc1$lon)
zc1$hh <- rep(NA,nrow(zc1))
zc1$hh[l11[!is.na(l11)]] <- zp1$hh[!is.na(l11)]
sum(i <- is.na(zc1$hh) | zc1$hh < 10 ) # 9068 small ZCTAs
#j=which(i)[1]; zc1[j,]; zc1[-i,][which.min( (zc1$lat[j]-zc1$lat[-i])^2 + (zc1$lon[j]-zc1$lon[-i])^2 ),]
k <- which(!i)[sapply( which(i), FUN=function(j) which.min( (zc1$lat[j]-zc1$lat[!i])^2 + (zc1$lon[j]-zc1$lon[!i])^2 ) )]
stopifnot( zc1$hh[k] >= 10 )
zc1$lat[i] <- zc1$lat[k]
zc1$lon[i] <- zc1$lon[k]

# now match the zips in foster address data:
table(!is.na(l11 <- match( a1$remzip5, zc1[,2] ))) # 1072 (1.6%) don't match
sort(table( as.character(a1$remzip5)[is.na(l11)], useNA='always' )) # 131 blank + erroneous
tail(sort(table( a1$remzcta <- zc1[l11,2], useNA='always' )))
summary(as.numeric(table( a1$remzcta )))
#30060 30349 30701 30161 30032  <NA> 
#  705   750   784   798   969  1048 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    2.00   16.00   75.18   89.00  969.00
tail(sort(table( a1$remzctalat <- zc1$lat[l11], useNA='always' )))
tail(sort(table( a1$remzctalon <- zc1$lon[l11], useNA='always' )))

table(!is.na(l11 <- match( a1$plczip5, zc1[,2] ))) # 5819 (8.5%) don't match
sort(table( as.character(a1$plczip5)[is.na(l11)], useNA='always' )) # 185 blank + erroneous
tail(sort(table( a1$plczcta <- zc1[l11,2], useNA='always' )))
summary(as.numeric(table( a1$plczcta )))
#30701 31907 30349 30058 30236  <NA> 
#  621   675   684   708   712  6014 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    1.00    3.00   41.22   30.00  712.00
tail(sort(table( a1$plczctalat <- zc1$lat[l11], useNA='always' )))
tail(sort(table( a1$plczctalon <- zc1$lon[l11], useNA='always' )))

# link a1 to AFCARS
require(chron)
system.time(load( '/fci/reports/ga/data/County_incare.RData' )) # 3s, d1, y, endofperiod, begofperiod
pointintime <- as.Date(endofperiod) #as.Date('2018-09-30')
d1 <- d1[order( as.numeric(d1[,21]), decreasing=T ),] # match most recent remdate
prop.table(table(!is.na(l11 <- match( a1$AFCARS_ID, d1[,4] )))) # 97.8%, 1703 unlinked
# todo: check unlinked.
a1$plctp <- d1$plctp[l11] # not using yet?
summary(a1$perdiem <- round( d1$MonthlyFCPayment[l11]/(365.25/12) )) # using this

# Accumulate indicators into frequency fields:
# too big: dim(x2 <- table( 'RemovalZip'=a1$remzip5[i1], 'RemovalCounty'=a1$REMOVAL_COUNTY[i1], 'PlacementZip20171208'=a1$plczip5[i1], 'PlacementCounty'=a1$PLACEMENT_COUNTY[i1] ))
# too big: dim(x2 <- xtabs(  ~ remzip5+REMOVAL_COUNTY+plczip5+PLACEMENT_COUNTY, data=a1[i1,] ))
#a1$p1 <- do.call( paste, c(a1[, c('remzip5','REMOVAL_COUNTY','plczip5','PLACEMENT_COUNTY')], sep=',') )
f1 <- c('remcofips','plccofips','plczcta','plczctalat','plczctalon')
a1$p1 <- do.call( paste, c(a1[, f1], sep='|') )

# limit to in foster care on lastday:
summary(a1$lastday <- as.Date( a1$LAST_DAY, format='%d-%b-%Y' ))
summary(a1$remdate <- as.Date( a1$REMOVAL_DATE, format='%d-%b-%Y' ))
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2001-06-06" "2012-03-14" "2015-05-26" "2014-12-31" "2018-03-05" "2019-01-03"
#"1905-01-01" "2011-03-07" "2014-03-31" "2013-09-18" "2016-06-30" "2019-01-02"
#table( i1 <- a1$lastday == max(a1$lastday) ) # 19.5%
table( i1 <- a1$remdate <= pointintime & (is.na(a1$lastday) | a1$lastday >= pointintime) )
#FALSE  TRUE 
#62227 13771 2018-09-30

# in care last day:
dim(x1 <- data.frame(xtabs( ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 13362
x <- strsplit( as.character(x1[,1]), '|', fixed=T )
head(x <- t(sapply( x,function(y) c(y[1:length(f1)]) )))
colnames(x) <- f1
x2 <- cbind( data.frame(x), x1$Freq )
units <- c( 'Children' )
names(units)[length(units)] <- colnames(x2)[ncol(x2)] <- paste('In Foster Care', pointintime)
str(x2)

# with family:
sort(table( a1$PLACEMENT_TYPE ))
#write.csv( data.frame(sort(table( a1$PLACEMENT_TYPE ))), '', quote=F )
nonfam <- !grepl( '^Relative|Parent|Non-Cust| Relative|Adopt', a1$PLACEMENT_TYPE, ignore.case=T )
sort(table( a1$PLACEMENT_TYPE[nonfam] ))
paste(names(sort(table( a1$PLACEMENT_TYPE[nonfam] ), decreasing=T)), collapse=', ')
paste(names(sort(table( a1$PLACEMENT_TYPE[!nonfam] ), decreasing=T)), collapse=', ')
# DFCS Family Foster Home, CPA Family Foster Home, Child Care Institution, , Other Person, Runaway, Other Resource, YDC/RYDC, Hospital, Group Home, Emergency Shelter, Non-Relative Paid, CCI Family Foster Home, ICPC - Foster, ILP/Aftercare, Specialized Foster Home
# Relative - Paid, Adoptive Home, Relative - Unpaid, Parent/Primary Caretaker, Relative Foster Home, Non-Custodial Parent, ICPC - Relative, ICPC - Adoptive
dim(x1 <- data.frame(xtabs( (!nonfam[i1]) ~ a1$p1[i1] ))) # 4691
sum(x1$Freq) # 4526
str(x2 <- cbind( x2, x1$Freq ))
(units <- c( units, 'Children' ))
names(units)[length(units)] <- colnames(x2)[ncol(x2)] <- paste('With Family', pointintime)
str(x2)

# with non-family:
dim(x1 <- data.frame(xtabs( nonfam[i1] ~ a1$p1[i1] ))) # 4691
sum(x1$Freq) # 9245
str(x2 <- cbind( x2, x1$Freq ))
(units <- c( units, 'Children' ))
names(units)[length(units)] <- colnames(x2)[ncol(x2)] <- paste('With Non-Family', pointintime)
str(x2)

# per-diems:
dim(x1 <- data.frame(xtabs( pmax(0,a1$perdiem[i1],na.rm=T) ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 730348
str(x2 <- cbind( x2, x1$Freq ))
(units <- c( units, 'Dollars' ))
names(units)[length(units)] <- colnames(x2)[ncol(x2)] <- paste('Total Per-Diem on', pointintime)
str(x2)

# todo: non-family per-diems?

x2[sample(nrow(x2),10),]
# probably ok for public?

require( rjson )
# for each indicator, need unit and name
js1 <- toJSON(list( data=x2, units=units ))
js1 <- gsub( '],', '\n],\n', js1 )
#js1 <- gsub( '\\[', '[\n', js1 )
js1 <- gsub( '"NA"', '""', js1 )
cat( 'var fd1 = ', js1, ' ;\n', file='GARemPlc1.js', sep='' ) # 208k

#write.csv( x2, '../GARemPlcZip20171208c.csv', row.names=F, quote=F )


