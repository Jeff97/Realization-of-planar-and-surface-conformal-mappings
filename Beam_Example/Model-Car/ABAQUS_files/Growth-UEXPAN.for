C234567890123456789012345678901234567890123456789012345678901234567890
      SUBROUTINE UEXPAN(expan,dexpandt,temp,time,dtime,predef,dpred,
     &     statev,cmname,nstatv,noel)
C
      INCLUDE 'ABA_PARAM.INC' 
C
      character*80 cmname
      dimension expan(*),dexpandt(*),temp(2),time(2),predef(*),
     &     dpred(*),statev(nstatv)
      
      REAL  Pi, Stage
      DOUBLE PRECISION :: lamuda(5688)

      SAVE lamuda
      data iread /1/
      SAVE iread

      INTEGER :: NumBotEleSlant, NumTopEleSlant, NumEleZ

      Pi = 3.14159265359
      ! 4192 Elements
      NumBotEleSlant = 2844
      NumTopEleSlant = 5688
      NumEleZ = 6721
      
      call MutexInit( 1 )
      call MutexLock( 1 )

      IF (iread .EQ. 1) THEN

      WRITE(*,*) "opening 301"
      open(301,FILE='C:\temp\complex_surface\Beetle\'//
     &  'lamuda.csv',status="old")
      read(301,*) lamuda
      close(301)
        
      iread=2
      END IF

      Stage=1.0

C temp(2) is Temperature increment
C Increments of thermal strain
       
      ! BotEleX, BotEleY, BotEleSlanT
      IF (noel .LE. NumBotEleSlant) THEN
      expan(1) = (lamuda(NOEL)-1.0)*temp(2)*Stage
      STATEV(4)=lamuda(NOEL)-1.0
      END IF
      ! TopEleX, TopEleY, TopEleSlanT
      IF (noel .GT. NumBotEleSlant .AND. noel .LE. NumTopEleSlant) THEN
      expan(1) = (lamuda(NOEL)-1.0)*temp(2)*Stage
      STATEV(4)=lamuda(NOEL)-1.0
      END IF
      ! EleZ
      IF (noel .GT. NumTopEleSlant .AND. noel .LE. NumEleZ) THEN
      expan(1) = 0.0
      STATEV(4)=0.0
      END IF

      call MutexUnLock( 1 )

C
C     dexpandt(1) = alpha1
C     dexpandt(2) = alpha2


      RETURN
      END 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SDVINI(STATEV,COORDS,NSTATV,NCRDS,NOEL,NPT,LAYER,KSPT)
C
      INCLUDE 'ABA_PARAM.INC' 
C
      DIMENSION STATEV(NSTATV),COORDS(NCRDS)
C
      IF (STATEV(1) .EQ. 0) THEN
        STATEV(1)=COORDS(1)
      END IF
      IF (STATEV(2) .EQ. 0) THEN
        STATEV(2)=COORDS(2)
      END IF
      IF (STATEV(3) .EQ. 0) THEN
        STATEV(3)=COORDS(3)
      END IF

      RETURN
      END 