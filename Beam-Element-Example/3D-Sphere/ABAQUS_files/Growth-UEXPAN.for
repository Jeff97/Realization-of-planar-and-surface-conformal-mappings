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
      DOUBLE PRECISION :: Botlamuda(7400),Toplamuda(7400)

      SAVE Botlamuda
      SAVE Toplamuda
      data iread /1/
      SAVE iread

      INTEGER :: NumBotEleSlant, NumTopEleSlant, NumEleZ

      Pi = 3.14159265359
      ! 100 Elements
      NumBotEleSlant = 7400
      NumTopEleSlant = 14800
      NumEleZ = 15641
      
      call MutexInit( 1 )
      call MutexLock( 1 )

      IF (iread .EQ. 1) THEN

      WRITE(*,*) "opening 301"
      open(301,FILE='C:\temp\Grow_of_beam\ABAQUS_Temp\LSCM\'//
     &  '3D-the\Slant_Sphere\400\5_elements\Botlamuda.csv',status="old")
      read(301,*) Botlamuda
      close(301)

      WRITE(*,*) "opening 302"
      open(302,FILE='C:\temp\Grow_of_beam\ABAQUS_Temp\LSCM\'//
     &   '3D-the\Slant_Sphere\400\5_elements\Toplamuda.csv',status="old")
      read(302,*)Toplamuda
      close(302)
        
      iread=2
      END IF

      Stage=1.0

C temp(2) is Temperature increment
C Increments of thermal strain
       
      ! BotEleX, BotEleY, BotEleSlanT
      IF (noel .LE. NumBotEleSlant) THEN
      expan(1) = (Botlamuda(NOEL)-1.0)*temp(2)*Stage
      STATEV(8)=Botlamuda(NOEL)-1.0
      END IF
      ! TopEleX, TopEleY, TopEleSlanT
      IF (noel .GT. NumBotEleSlant .AND. noel .LE. NumTopEleSlant) THEN
      expan(1) = (Toplamuda(NOEL-7400)-1.0)*temp(2)*Stage
      STATEV(8)=Toplamuda(NOEL-7400)-1.0
      END IF
      ! EleZ
      IF (noel .GT. NumTopEleSlant .AND. noel .LE. NumEleZ) THEN
      expan(1) = 0.0
      STATEV(8)=0.0
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