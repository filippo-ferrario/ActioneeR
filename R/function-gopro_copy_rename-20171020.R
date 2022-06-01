#===============================================================================
# Name   : 
# Author : Filippo Ferrario
# Date   : 02/10/2017 09.41.31
# Version: v1
#===============================================================================

#===============================
# ideas
#===============================


# Arg list:
#-------------
# 1) path for where to find the files
# 2) path for where to copy the file
# 3) path for where to find the field.data spreadsheet with the info on wich file to copy.
# 4) path for where to find the 'gopro files metadata.txt' file
# 5) selection criteria for sorting the gopro file metadata list : coded as "by=c('col1=value1;value2;value3;..;valueN', 'col3=value')"
# 6) specifie the name tag structure : probably as a character string such as "station_replicate" where "station" and "raplicate"  are column names in the variable part of the field data spreadsheet.

# what the function should do:
#-------------------------------
#
#1) SELECT THE FILES
#   read the field.data spreadsheet to check which files need to be copied and renamed.
#   a- IN CASE OF SEQUENCE: 
#      i  - select all : coded "all"
#      ii - select pictures at regular interval : "e2" for every 2       
#      iii- select a specific one : insert the number of the specific one
#      iv - select a range : coded as "2-10"
#      v  - select more than one by the user: coded as "2;4;7;12"     
#      vi - ONLY for vds- all the above but only for pics: coded as "op:"followed by the othe code e.g. "op:all" "op:e3"....
#      
#2) read the metadata
#3) prepare de name tag using the structure specified by the user,
#   a- IF a file belong to a sequence, need to attach a tag to identify which position the file had in the sequence
#   b- attach the original file name at the end
#4) copy and rename the files one at the time in a loop.
#



#####

## agrs
#
#source.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/100GOPRO_primo'
##source.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/prova seqcounter'
#field.data.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro files.xlsx'
#gopro.meta.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/gopro files metadata.txt'
#dest.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/copia di prova'
#sheetName='field.data'
#startRow=13
#date.from='28/09/2017' # format='%d/%m/%Y'  ; specify this at least
#date.to=NULL    #format='%d/%m/%Y'
#
#filters=c('site=fake1,bay,green', 'camera.ID=hero4')
##filters=c('site=fake1,bay,green', 'station=b1')
#name.str=c('site_station_replicate')
#
## function body:


gopro.copyRename<-function(source.dir,dest.dir,field.data.path,sheetName,startRow,gopro.meta.path,date.from=NULL,date.to=NULL, filters=NULL,name.str ){


file.meta<-read.table(gopro.meta.path, sep='\t', head=T, colClasses="character")

require(xlsx)
field.data<- read.xlsx(field.data.path, sheetName, startRow=startRow, as.data.frame=TRUE, header=TRUE,stringsAsFactors = FALSE )

names(field.data)<- tolower(names(field.data))

# turn all fix columns to lower case  
fix.col<-c("date","camera.model", "camera.id","type","frequency","video.length","to.copy" )

for(i in 1: length(fix.col)) {
     field.data[,fix.col[i]]<-tolower(field.data[,fix.col[i]])
} 

field.data.names<- names(field.data)

##

{
# not sure I need this chunk
# subset the field.dataset to take into account only pictures taken on specific dates (usually one or a range)

field.data$date<-as.Date(field.data$date)

if(!is.null(date.from)){date.from<-as.Date(date.from, format='%d/%m/%Y')}
if(!is.null(date.to)){date.to<-as.Date(date.to, format='%d/%m/%Y')}


if (!is.null(date.from) & !is.null(date.to)){field.sub<-field.data[field.data$date>=date.from  &field.data$date<=date.to,]} 
if (!is.null(date.from) & is.null(date.to)){field.sub<-field.data[field.data$date>=date.from,]} 
if (is.null(date.from)  & is.null(date.to)){field.sub<-field.data} 
                                                                           
}

## user specific filtering:
## parse filters 


FILTER<-function(criteria, dataset){
          data.sub<- dataset
          filt.list<-strsplit(criteria, split='=')
          filt.levels.list<-sapply(filt.list,FUN=function(x){strsplit(x[2], split=',')})
            for (i in 1: ncol(dataset)) {  # select coloumns
                for(k in 1:length(filt.list)){ # select rows
                    if (!('NA'%in%filt.levels.list[[k]]) & names(data.sub)[i]==filt.list[[k]][1]){
                    #if (names(dataset)[i]==filt.list[[k]][1]){
                    r.id<-which(data.sub[,i]%in%filt.levels.list[[k]])
                    data.sub<-data.sub[r.id,]
                    }
                }
            }
          return(data.sub)  
}

# parse info in the cell "to.ccpy"
FILTER.seq<-function(crit, dataset.seq){
            data.seq.sub<- dataset.seq
            
            if (crit=='all') {return(data.seq.sub)} 
               # intervals e.g "e3"
            if (grepl(crit, pattern='e[[:digit:]]')) { 
                  f<-as.numeric(strsplit(crit,split='e')[[1]][2])
                  r.id<-seq(from=0,to=nrow(data.seq.sub), by=f)
                  data.seq.sub<-data.seq.sub[r.id,]
                  return(data.seq.sub)
                  } 
               # specific number
            num<-as.numeric(crit)
            if (!is.na(num)){
                  r.id<-num
                  data.seq.sub<-data.seq.sub[r.id,]
                  return(data.seq.sub)
                  }
               #  "2-3-6"  
            if ( !is.na( as.numeric(strsplit(crit, split='-')[[1]])  ) ) {
                 from<-as.numeric(strsplit(crit, split='-')[[1]][1])
                 to  <-as.numeric(strsplit(crit, split='-')[[1]][2])
                 data.seq.sub<-data.seq.sub[from:to,]
                 return(data.seq.sub)
            }   
               #    
            if ( !is.na( as.numeric(strsplit(crit, split=';')[[1]])  ) ) {
                 r.id <-as.numeric(strsplit(crit, split=';')[[1]])
                 data.seq.sub<-data.seq.sub[r.id,]
                 return(data.seq.sub)
            }   

}




# filter the field data on the critaria the user define
if (!is.null(filters)){
   field.sub<-FILTER(criteria=filters, dataset=field.sub )}
# keep only the rows selected for copy
field.sub<- field.sub[!is.na(field.sub$to.copy),]


## loop to select the file to be copied


# initialize the dataframe where to store results of selection loop
meta.sub<-file.meta[0,]

for (r in 1:nrow(field.sub)){

typ<- field.sub$type[r] 

if (typ%in%c('video','sing')){
#  new.filters<-c(paste0('type=',typ),filters)
#  meta.sub<-rbind(meta.sub,FILTER(criteria=new.filters, dataset=file.meta ))
 
   {#10/10/2017 12.16.53
  # filtro per riga per leggere le diverse condizioni
   row.filt<-sapply(names(field.sub[r,]), FUN= function(x){ paste0(x,'=',field.sub[r,x]) } )
   #seq.sub<-FILTER(criteria=row.filt, dataset=file.meta )
   meta.sub<-rbind(meta.sub,FILTER(criteria=row.filt, dataset=file.meta ))
   }

}


if (typ%in%c('cont','burst','timelapse')){
   # filtro per riga per leggere le diverse condizioni
   row.filt<-sapply(names(field.sub[r,]), FUN= function(x){ paste0(x,'=',field.sub[r,x]) } )
   seq.sub<-FILTER(criteria=row.filt, dataset=file.meta )
   meta.sub<-rbind(meta.sub,FILTER.seq(crit=field.sub$to.copy[r], dataset.seq=seq.sub))
} 

if (typ=='vds'){
   row.filt<-sapply(names(field.sub[r,]), FUN= function(x){ paste0(x,'=',field.sub[r,x]) } )
   seq.sub<-FILTER(criteria=row.filt, dataset=file.meta )
   sub.JPG<- seq.sub[-1,]
   #sub.JPG<- seq.sub[ grep(seq.sub$file,pattern='.JPG'),]
    
    if (grepl(field.sub$to.copy[r], pattern='op:')){ 
     new.crit<-strsplit(field.sub$to.copy[r], split='op:')[[1]][2]
     meta.sub<-rbind(meta.sub,FILTER.seq(crit=new.crit, dataset.seq=sub.JPG))    
     } else {
     meta.sub<-rbind(meta.sub,seq.sub[1,])    
     meta.sub<-rbind(meta.sub,FILTER.seq(crit=field.sub$to.copy[r], dataset.seq=sub.JPG))    
    }
}

}        


## Prepare the name tag

nam.comp<-strsplit(name.str, split='_')[[1]]

for (n in 1:nrow(meta.sub)){
   
   if (meta.sub$type[n] %in% c('sing','video')){
      #c.id<- which(names(meta.sub)%in%nam.comp)
      new.comp<-unlist(c(meta.sub[n,nam.comp],meta.sub[n,'file']))
      new.comp<-new.comp[!is.na(new.comp)]
      tag<-paste0(new.comp,collapse='_')
   }
   if (meta.sub$type[n] %in% c('cont','burst','timelapse','vds')){
      #c.id<- which(names(meta.sub)%in%c(nam.comp))
      new.comp<- if ( is.na(meta.sub[n,'shot'])) { unlist(c(meta.sub[n,nam.comp],meta.sub[n,'file']))} else { 
                         unlist(c(meta.sub[n,nam.comp],paste0('pic#',meta.sub[n,'shot']),meta.sub[n,'file']))}
      new.comp<-new.comp[!is.na(new.comp)]                    
      tag<-paste0(new.comp,collapse='_')
   }

  if (! dir.exists (dest.dir)) { dir.create (dest.dir)}                           

  file.copy(from=paste0(source.dir,'/',meta.sub$file[n] ),to=dest.dir)
  file.rename( from=paste0(dest.dir,'/',meta.sub$file[n] ), 
               to=paste0(dest.dir,'/',tag))
      

}



}


### bench


#FILTER(filters, file.meta)
#FILTER(filters, field.data)

##
#
#source.dir='E:/Chesterfield Inlet/CI Hero4 20171001' 
#dest.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/data/photos'
#field.data.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/data/Scallops mapping.xlsx'
#sheetName='field.data'
#startRow=9
#gopro.meta.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/data/gopro files metadata.txt'
#date.from='01/10/2017'
#date.to='01/10/2017'
#filters=NULL
#name.str='station_replicate_pic.type'
