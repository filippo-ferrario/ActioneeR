
#
#13/10/2017 16.44.41

# Errors for possible fix:
# - errore dovuto a dati in fiel.data sbagiati (i.e. iserito uno scatto che non era stato fatto (me ne sono accorto tramite numero progressivo ripetuto anche sui fogli originali))

# - nome cartella source sbagliato
# gopro.meta(  source.dir='H:/BSI/Sampling/hero4 red up to 20170803', 
#+             dest.dir='D:/Lavoro/LAVAL/Contratti PHIL/algal survey 2017 per IN .... [TRUNCATED] 
#Error in if (x < 10) { : valore mancante dove è richiesto TRUE/FALSE


# always get this message
#Wanrings()
#: In data.frame(..., check.names = FALSE) :
#  row.names ricavati da una variabile con pochi elementi e quindi non utlizzabili



#
#### function args:       
#
##source.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/100GOPRO_primo'
#source.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/prova seqcounter'
#field.data.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro files.xlsx'
#dest.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/'
#sheetName='field.data'
#startRow=13
#camera.ID='hero4'
#date.from='28/09/2017' # format='%d/%m/%Y'  ; must be specified
#date.to=NULL           # format='%d/%m/%Y'  ; must be specified
#


#####  FUNCTION BODY:

gopro.meta<-function (source.dir, field.data.path,dest.dir,sheetName,startRow,camera.ID=NULL,date.from=NULL, date.to=NULL){


gopro.files<-dir(source.dir)

JPGseq<-gopro.files[ grep(gopro.files,pattern='G[0-9]{7}\\.JPG')]
JPGsin<-gopro.files[ grep(gopro.files,pattern='GOPR[0-9]{4}\\.JPG')]
MP4<-gopro.files[ grep(gopro.files,pattern='\\.MP4')]

## read the data collected in the field

require(xlsx)
field.data<- read.xlsx(field.data.path, sheetName, startRow=startRow, as.data.frame=TRUE, header=TRUE,stringsAsFactors = FALSE)

names(field.data)<- tolower(names(field.data))
# turn all fix columns to lower case  
fix.col<-c("date","camera.model", "camera.id","type","frequency","video.length","to.copy" )

for(i in 1: length(fix.col)) {
     field.data[,fix.col[i]]<-tolower(field.data[,fix.col[i]])
} 

field.data.names<- names(field.data)
##


{# subset the field.dataset to take into account only pictures taken on specific dates (usually one or a range)
field.data$date<-as.Date(field.data$date)

if(!is.null(date.from)){date.from<-as.Date(date.from, format='%d/%m/%Y')}
if(!is.null(date.to)){date.to<-as.Date(date.to, format='%d/%m/%Y')}


if (!is.null(date.from) & !is.null(date.to)){field.datasub<-field.data[field.data$date>=date.from  &field.data$date<=date.to,]} 
if (!is.null(date.from) & is.null(date.to)){field.datasub<-field.data[field.data$date>=date.from,]} 

# filter on camera.ID 
if (!is.null(camera.ID)){
field.datasub<-field.datasub[field.datasub$camera.id==tolower(camera.ID),]
}
     # qui mettere condizioni per filtrare su campi variabili (tipo, sito o diver)  come per la funzione di HLO

}



{#prepare the dataframe to store results
file.meta<-as.data.frame(matrix(ncol=length(field.data.names)+3, nrow=0))
names(file.meta)<-c(field.data.names,'shot','file','path')
}

# filling the file.meta dataframe

MP4.counter<-1
JPGsin.counter<-1
#JPGseq.counter<-1
JPGseq.counter<- as.numeric(substring(JPGseq[1],2,4)) # this makes the counter for sequences start from the first sequence found in the directory where the files are read from. This is needed when the images on the gopro are downloaded in different directories: in this case it is possible that the first sequence in a directory does not start from 001 but from another position (e.g. "005"). 



unrooted.source <- sub(x=source.dir, pattern='.:', replacement='' )


# define function to fill single shot and video
       # based on this:
       #  path<-paste0(source.dir,'/',JPGsin[JPGsin.counter])
       #  new.rows<- cbind(field.datasub[k,],shot=NA,file=JPGsin[JPGsin.counter],path=path,stringsAsFactors = FALSE  )
       #  file.meta<-rbind(file.meta,new.rows)
       #  JPGsin.counter<-JPGsin.counter+1
   
fill_sing_video<-function(f.names,counter.name='counter') {
                     counter<-get(counter.name)
                     path<-paste0(unrooted.source,'/',f.names[counter])
                     new.rows<- cbind(field.datasub[k,],shot=NA,file=f.names[counter],path=path,stringsAsFactors = FALSE  )
                     #file.meta<-rbind(file.meta,new.rows)
                     counter<-counter+1
                     assign(x=counter.name, value=counter, inherits=T)
                     return(new.rows)                  
                   } 
# define pattern for name matching in grep for sequences
match.seq<-function(x){ if(x<10) {patrn<-paste0('G00',x)} else{
                               if(x<100) {patrn<-paste0('G0',x)} else{
                                  if(x>=100) {patrn<-paste0('G',x)}
                                  }
                            }
                          return(patrn)
                          }
# define function to fill sequential shot 
     # based on this:
     #  patrn<-match.seq(JPGseq.counter)  
     #  seq.names<-JPGseq[grep(JPGseq,pattern=patrn)]
     #  path<-paste0(source.dir,'/',seq.names)
     #  new.rows<- cbind(field.datasub[k,],shot=c(1:length(path)),file=seq.names,path=path,stringsAsFactors = FALSE )
     #  file.meta<-rbind(file.meta,new.rows)
     #  JPGseq.counter<-JPGseq.counter+1

fill_seq<-function(f.names,counter.name='counter') {                                             
                     counter<-get(counter.name)
                     patrn<-match.seq(counter)  
                     seq.names<-f.names[grep(f.names,pattern=patrn)]
                     path<-paste0(unrooted.source,'/',seq.names)
                     new.rows<- cbind(field.datasub[k,],shot=c(1:length(path)),file=seq.names,path=path,stringsAsFactors = FALSE )
                     counter<-counter+1
                     assign(x=counter.name, value=counter, inherits=T)
                     return(new.rows) 
                    }

for(k in 1:nrow(field.datasub)){
 
  # case: sing 
  if (field.datasub[k,]$type=='sing'){
   file.meta<-rbind(file.meta,fill_sing_video(JPGsin,'JPGsin.counter'))
  }
  # case: video 
  if (field.datasub[k,]$type=='video'){
   file.meta<-rbind(file.meta,fill_sing_video(MP4,'MP4.counter'))
  }
                          
  # case: burst or timelapse or cont
  if (field.datasub[k,]$type%in%c('burst','cont','timelapse')){
   file.meta<-rbind(file.meta,fill_seq(JPGseq,'JPGseq.counter'))
  }
  
  # case: video + photo
  if (field.datasub[k,]$type=='vds'){
    # video
      file.meta<-rbind(file.meta,fill_sing_video(MP4,'MP4.counter'))
    # related photo
    seq.check<- ifelse( (field.datasub[k,]$video.length/field.datasub[k,]$frequency)>=1, TRUE,FALSE )
    if(seq.check) {
      file.meta<-rbind(file.meta,fill_seq(JPGseq,'JPGseq.counter'))
    }
  }
}


file.meta<-file.meta[,names(file.meta)!='to.copy']

if(file.exists(paste0(dest.dir,'/gopro files metadata.txt'))){
     write.table(file.meta, paste0(dest.dir,'/gopro files metadata.txt'), append=T,sep='\t',row.names=F,col.names=F, quote=F)
    } else{
    write.table(file.meta, paste0(dest.dir,'/gopro files metadata.txt'), append=F,sep='\t',row.names=F,col.names=T, quote=F)
    }


}


#  bench
# ==========
###
#
#source.dir='E:/Chesterfield Inlet/CI Hero4 20171001'
#dest.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/data' 
#field.data.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/data/Scallops mapping.xlsx' 
#sheetName='field.data' 
#startRow=9 
#camera.ID = NULL 
#date.from = '01/10/2017' 
#date.to = '01/10/2017'
#


#gopro.meta( source.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/100GOPRO_primo',
#            field.data.path='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro files.xlsx',
#            dest.dir='D:/Lavoro/LAVAL/Contratti PHIL/Chesterfiel Nunavut - scallops/prove gopro/',
#            sheetName='field.data',
#            startRow=13,
#            camera.ID='hero4',
#            date.from='28/09/2017', # format='%d/%m/%Y'  ; specify this at least
#            date.to=NULL    #format='%d/%m/%Y',
#            )

#
#
#
#
#
#
#
#
#
#
#read.table(paste0(dest.dir,'pic database.txt'), head=T, sep='\t' )






