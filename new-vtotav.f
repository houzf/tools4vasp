      PROGRAM VTOTAV
C     original author: unknown; may be released by VASP group
C     modified by  HOU Zhufeng: valenhou@gmail.com      
C     variables are allocated dynamically
C     suitable for LOCPOT written by VASP 4.6 & 5.X both     
C     the number of atom species in POSCAR is less than 8; otherwise
C     modify the following 3 lines:
C     I=0; II=0; III=0; IIII=0; IV=0; IVI=0; IVII=0; IVIII=0
C     READ(HEADER,*,ERR=12,END=12) I,II,III,IIII, IV, IVI, IVII, IVIII
C12   NIONS=I+II+III+IIII+IV+IVI+IVII+IVIII
C or      
C13   NIONS=I+II+III+IIII+IV+IVI+IVII+IVIII
      
      CHARACTER*80 HEADER
      REAL*8, ALLOCATABLE:: VLOCAL(:),VAV(:)
      I=0

      WRITE(*,*) 'Which direction to keep? (1-3 --- 1=X,2=Y,3=Z)'
      READ(*,*) IDIR
      IDIR=MOD(IDIR+20,3)+1
      OPEN(20,FILE='LOCPOT',STATUS='OLD',ERR=1000)
C      READ(20,*,ERR=1000,END=1000) NIONS,IDUM1,IDUM2
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      I=0; II=0; III=0; IIII=0; IV=0; IVI=0; IVII=0; IVIII=0
      READ(HEADER,*,ERR=12,END=12) I,II,III,IIII, IV, IVI, IVII, IVIII
12    NIONS=I+II+III+IIII+IV+IVI+IVII+IVIII
CCCC  NIONS > 0   --->  LOCPOT is written by vasp4.6
CCC   NIONS = 0   --->  LOCPOT is written by vasp5.x, so
CCC   one more line should be read !      
      IF ( NIONS > 0 ) THEN
          WRITE(*,*) 'LOCPOT is written by VASP 4.6'
       ELSE
         write(*,*) 'LOCPOT is written by VASP 5.x'
         I=0; II=0; III=0; IIII=0; IV=0; IVI=0; IVII=0; IVIII=0
         READ(20,'(A)',ERR=1000,END=1000) HEADER
         READ(HEADER,*,ERR=13,END=13) I,II,III,IIII, IV, IVI, IVII,IVIII
13     NIONS=I+II+III+IIII+IV+IVI+IVII+IVIII
      END IF

C     READ(20,'(A)',ERR=1000,END=1000) HEADER
C     READ(20,*,ERR=1000,END=1000) NIONS
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      WRITE(*,*) 'Number of ions:', NIONS
      DO  I=1,NIONS
         READ(20,*,ERR=1000,END=1000) RDUM1,RDUM2,RDUM3
      END DO
      WRITE(*,*) 'positions read'
      READ(20,'(A)',ERR=1000,END=1000) HEADER
      READ(20,*,ERR=1000,END=1000) NGX,NGY,NGZ
      NPLWV=NGX*NGY*NGZ
      IF (IDIR.EQ.1) NOUT=NGX
      IF (IDIR.EQ.2) NOUT=NGY
      IF (IDIR.EQ.3) NOUT=NGZ
      ALLOCATE(VLOCAL(NPLWV))
      ALLOCATE(VAV(NOUT+10))
C      READ(20,'(10F8.3)',ERR=1000,END=1000) (VLOCAL(I),I=1,NPLWV)
      READ(20,*,ERR=1000,END=1000) (VLOCAL(I),I=1,NPLWV)
      WRITE(*,*) 'charge density read'
      CLOSE(20)
      VAV(:)=0.
      SCALE=1./FLOAT(NPLWV/NOUT)
      WRITE(*,*) SCALE
      IF (IDIR.EQ.1) THEN
         DO  IX=1,NGX
            DO  IZ=1,NGZ
             DO  IY=1,NGY
               IPL=IX+((IY-1)+(IZ-1)*NGY)*NGX
               VAV(IX)=VAV(IX)+VLOCAL(IPL)*SCALE
             END DO
            END DO
         END DO
      ELSE IF (IDIR.EQ.2) THEN
         DO  IY=1,NGY
            DO  IZ=1,NGZ
             DO  IX=1,NGX
               IPL=IX+((IY-1)+(IZ-1)*NGY)*NGX
               VAV(IY)=VAV(IY)+VLOCAL(IPL)*SCALE
             END DO
            END DO
         END DO
      ELSE IF (IDIR.EQ.3) THEN
         DO  IZ=1,NGZ
            DO  IY=1,NGY
             DO  IX=1,NGX
               IPL=IX+((IY-1)+(IZ-1)*NGY)*NGX
               VAV(IZ)=VAV(IZ)+VLOCAL(IPL)*SCALE
             END DO
            END DO
         END DO
      ELSE
         WRITE(*,*) 'Hmmm?? Wrong IDIR ',IDIR
         STOP
      ENDIF
      OPEN(20,FILE='VLINE')
      WRITE(20,*) "##########",NOUT,IDIR
      DO  I=1,NOUT
         WRITE(20,'(I6,2X,E18.11)') I,VAV(I)
      END DO
      CLOSE(20)
 1000 WRITE(*,*) 'Error opening or reading file LOCPOT.'
      WRITE(*,*) 'item :',I
      DEALLOCATE(VAV)
      DEALLOCATE(VLOCAL)
      END PROGRAM

