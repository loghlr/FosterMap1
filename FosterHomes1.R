# abb 12/28/2018: Tally foster home boarding county/zip by removal county (not zip)

stabb='GA'
a1 <- read.table( '/fci/reports/ga/data/geocodes/recd20171208/afcars addresses 2017dec08.dsv.xz', header=T, sep='|', as.is=T, comment.char='', quote='"' )
str( a1 ) # 68601 obs. of  16 variables
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
table( a1$remcofips <- c1$fips[l11], useNA='always' )

sort(table(cn1 <- paste( stabb, toupper(sub( ' +(parish|county|borough)$', '', a1$PLACEMENT_COUNTY, ignore.case=T ))) ))
table(!is.na(l11 <- match( cn1, c1$CountyName ))) # 6607 (9.6%) don't match
table( cn1[is.na(l11)], useNA='always' ) # all none, na or out of state
table( a1$plccofips <- c1$fips[l11], useNA='always' )

# convert zips to zcta centroids.
tail(sort(table( a1$remzip5, useNA='always' )))
table( nchar(as.character(a1$remzip5)), useNA='always' )
#30060 30701 30349 30161 30032       
#  662   682   697   747   906   937 
#    0     5  <NA> 
#  937 67664     0

str(zc1 <- read.table( '/fci/reports/us/census/geonameszip_US.txt.xz', sep='\t', header=F, as.is=T, colClasses='character' ))
colnames(zc1) <- unlist(strsplit('co zip zipname statename statefips countyname countyfips communityname communityfips lat lon accuracy',' '))

table(!is.na(l11 <- match( a1$remzip5, zc1[,2] ))) # 1072 (1.6%) don't match
sort(table( as.character(a1$remzip5)[is.na(l11)], useNA='always' )) # 131 blank + erroneous
tail(sort(table( a1$remzcta <- zc1[l11,2], useNA='always' )))
summary(as.numeric(table( a1$remzcta )))
#30060 30701 30349 30161 30032  <NA> 
#  662   682   697   747   906  1072 
#> summary(as.numeric(table( a1$remzcta )))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    2.00   17.00   70.49   84.00  906.00
tail(sort(table( a1$remzctalat <- zc1$lat[l11], useNA='always' )))
tail(sort(table( a1$remzctalon <- zc1$lon[l11], useNA='always' )))

table(!is.na(l11 <- match( a1$plczip5, zc1[,2] ))) # 5819 (8.5%) don't match
sort(table( as.character(a1$plczip5)[is.na(l11)], useNA='always' )) # 185 blank + erroneous
tail(sort(table( a1$plczcta <- zc1[l11,2], useNA='always' )))
summary(as.numeric(table( a1$plczcta )))
#30127 30236 30349 31907 30058  <NA> 
#  576   592   612   619   666  5819 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00    1.00    3.00   39.56   31.00  666.00
tail(sort(table( a1$plczctalat <- zc1$lat[l11], useNA='always' )))
tail(sort(table( a1$plczctalon <- zc1$lon[l11], useNA='always' )))

# link a1 to AFCARS
system.time(load( '/fci/reports/ga/data/County_incare.RData' )) # 3s, d1, y, endofperiod, begofperiod
prop.table(table(!is.na(l11 <- match( a1$AFCARS_ID, d1[,4] )))) # 98.9%, 728 unlinked
# todo: check unlinked.
a1$plctp <- d1$plctp[l11]
summary(a1$perdiem <- round( d1$MonthlyFCPayment[l11]/(365.25/12) ))

# Accumulate indicators into frequency fields:
# too big: dim(x2 <- table( 'RemovalZip'=a1$remzip5[i1], 'RemovalCounty'=a1$REMOVAL_COUNTY[i1], 'PlacementZip20171208'=a1$plczip5[i1], 'PlacementCounty'=a1$PLACEMENT_COUNTY[i1] ))
# too big: dim(x2 <- xtabs(  ~ remzip5+REMOVAL_COUNTY+plczip5+PLACEMENT_COUNTY, data=a1[i1,] ))
#a1$p1 <- do.call( paste, c(a1[, c('remzip5','REMOVAL_COUNTY','plczip5','PLACEMENT_COUNTY')], sep=',') )
f1 <- c('remcofips','plccofips','plczcta','plczctalat','plczctalon')
a1$p1 <- do.call( paste, c(a1[, f1], sep='|') )

# limit to in foster care on lastday:
summary(a1$lastday <- as.Date( a1$LAST_DAY, format='%d-%b-%Y' ))
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2001-06-06" "2011-11-17" "2014-10-16" "2014-06-04" "2017-05-17" "2017-12-08"
table( i1 <- a1$lastday == max(a1$lastday) ) # 19.5%
#FALSE  TRUE 
#55239 13362

# in care last day:
dim(x1 <- data.frame(xtabs( ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 13362
x <- strsplit( as.character(x1[,1]), '|', fixed=T )
head(x <- t(sapply( x,function(y) c(y[1:length(f1)]) )))
colnames(x) <- f1
str(x2 <- cbind( data.frame(x), 'In Foster Care 2017-12-08'=x1$Freq ))
(units <- c( 'In Foster Care 2017-12-08'='Children' ))

# with family:
sort(table( a1$PLACEMENT_TYPE ))
nonfam <- !grepl( '^Relative|Parent|Non-Cust| Relative|Adopt', a1$PLACEMENT_TYPE, ignore.case=T )
sort(table( a1$PLACEMENT_TYPE[nonfam] ))
dim(x1 <- data.frame(xtabs( (!nonfam[i1]) ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 4324
str(x2 <- cbind( x2, 'With Family 2017-12-08'=x1$Freq ))
(units <- c( units, 'With Family 2017-12-08'='Children' ))

# with non-family:
dim(x1 <- data.frame(xtabs( nonfam[i1] ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 9038
str(x2 <- cbind( x2, 'With Non-Family 2017-12-08'=x1$Freq ))
(units <- c( units, 'With Non-Family 2017-12-08'='Children' ))

# per-diems:
dim(x1 <- data.frame(xtabs( pmax(0,a1$perdiem[i1],na.rm=T) ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 730348
str(x2 <- cbind( x2, 'Total Per-Diem on 2017-12-08'=x1$Freq ))
(units <- c( units, 'Total Per-Diem on 2017-12-08'='Dollars' ))

todo:
# family per-diems:
dim(x1 <- data.frame(xtabs( pmax(0,a1$perdiem[i1],na.rm=T) ~ a1$p1[i1] ))) # 4628
sum(x1$Freq) # 730348
str(x2 <- cbind( x2, 'Total Family Per-Diem on 2017-12-08'=x1$Freq ))
(units <- c( units, 'Total Family Per-Diem on 2017-12-08'='Dollars' ))

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


