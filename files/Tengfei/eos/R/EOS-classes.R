##--------------------------------------------------------------##
##                     Classes
##--------------------------------------------------------------##

setClassUnion('numericOrNULL',c('numeric','NULL'))

setClassUnion('characterOrNULL',c('character','NULL'))

setClass('EOS',representation('VIRTUAL',
                              elementMetadata='ANY',
                              elementType='character'),
         prototype(elementType='ANY'))

setClass('GraphicPars',representation(pars='environment'))

setClass('EOSTrack',contains=c('GraphicPars'),
         representation(data='RangedData',
                        type='character'),
         prototype(pars=new.env()))

setClass('EOSView',contains=c('EOS','GraphicPars'),
         representation(
                     listData='list',
                   globalmap='RangedData'     
                     ),
         prototype(elementType='EOSTrack',
                   pars=new.env()
                   ))


     
