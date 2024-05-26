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
      DOUBLE PRECISION :: 
     & Lambda1, Lambda2,
     & LambdaXBot,  LambdaXTop, LambdaYBot, LambdaYTop

      INTEGER :: NumBotEleX, NumBotEleY, NumTopEleX, NumTopEleY
      INTEGER :: NumEleZ

      Pi = 3.14159265359
      ! 400 Elements
      NumBotEleX = 2100
      NumBotEleY = 4200
      NumTopEleX = 6300
      NumTopEleY = 8400
      NumEleZ = 8841
      
      Lambda1 = 0.5*SQRT(EXP(Pi*(STATEV(1)-1.0)))*Pi
      Lambda2 = 0.5*SQRT(EXP(Pi*(STATEV(1)-1.0)))*Pi

      LambdaXBot = Lambda1
      LambdaYBot = Lambda2
      LambdaXTop = Lambda1
      LambdaYTop = Lambda2

      Stage=1.0

C temp(2) is Temperature increment
C Increments of thermal strain
      ! BotEleX
      IF (noel .LE. NumBotEleX) THEN
      expan(1) = (LambdaXBot-1.0)*temp(2)*Stage
      STATEV(8)=(LambdaXBot-1.0)
      END IF
      ! BotEleY
      IF (noel .GT. NumBotEleX .AND. noel .LE. NumBotEleY) THEN
      expan(1) = (LambdaYBot-1.0)*temp(2)*Stage
      STATEV(8)=(LambdaYBot-1.0)
      END IF
      ! TopEleX
      IF (noel .GT. NumBotEleY .AND. noel .LE. NumTopEleX) THEN
      expan(1) = (LambdaXTop-1.0)*temp(2)*Stage
      STATEV(8)=(LambdaXTop-1.0)
      END IF
      ! TopEleY
      IF (noel .GT. NumTopEleX .AND. noel .LE. NumTopEleY) THEN
      expan(1) = (LambdaYTop-1.0)*temp(2)*Stage
      STATEV(8)=(LambdaYTop-1.0)
      END IF
      ! EleZ
      IF (noel .GT. NumTopEleY .AND. noel .LE. NumEleZ) THEN
      expan(1) = 0.0
      STATEV(8)=0.0
      END IF

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