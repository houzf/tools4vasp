c    +---------------------------------------------------------
c    |  For generating k-points along the high-symmetry lines in
c    |  Brillouin zone and for calculate band-structures !
c    |
c    |  OK for "VASP" package 
c    |                                Modified by   Hou Zhufeng 
c    +----------------------------------------------------------
C    ---------'syml'---------
c    6                     : nhighk
c    20  20  20  10  20    : ndiv(i)
c    X 0.5    0.0    0.5   : labhk(1),phighk(1,1),........
c    G 0.0    0.0    0.0
c    L 0.5    0.5    0.5
c    W 0.5    0.25   0.75
c    K 0.375  0.375  0.75
c    G 0.0    0.0    0.0
cc   0.000  2.449  2.449  -0.204158033  0.204158033  0.204158033
cc   2.449  0.000  2.449   0.204158033 -0.204158033  0.204158033
cc   2.449  2.449  0.000   0.204158033  0.204158033 -0.204158033   
c    -20.0  20.0           :emin, emax
c    0.0                   :efermi
c    direct & reciprocal lattice vectors over 'emin, emax' line
C    -----------------------
c     max k-points = 200
      program  gk 
      implicit real*8 (a-h,o-z)
      character*2 labhk
      dimension tkpt(200,3),pk(200,3),phighk(10,3)
      dimension disk(200),dish(10),labhk(10)
      dimension ndiv(10)
c
      open(5,file='syml',status='old')
      open(7,file='inp.kpt')
      open(8,file='KPOINTS')
c
      read(5,*) nhighk
      read(5,*) (ndiv(i),i=1,nhighk-1)

      do i=1,nhighk-1
            ntkp=ntkp+ndiv(i)
      enddo
      ntotkpt=ntkp+1  

      if(nhighk>10)then
        write(*,*)'Number of high-symmetry k points must < 10!'
        STOP
      endif
      if(ntotkpt>200)then
        write(*,*)'Total number of k points must <= 200!'
        STOP
      endif

      do i=1, nhighk
         read(5,*)  labhk(i),(phighk(i,j),j=1,3)
      enddo
      write(*,*) (labhk(i),i=1,nhighk)
c
c----- generating k-points along high symmetric lines --------
c
c
      pk(1,1)=phighk(1,1)
      pk(1,2)=phighk(1,2)
      pk(1,3)=phighk(1,3)
      ii=1
       do i = 2, nhighk
         delx = (phighk(i,1) - phighk(i-1,1))/float(ndiv(i-1))
         dely = (phighk(i,2) - phighk(i-1,2))/float(ndiv(i-1))
         delz = (phighk(i,3) - phighk(i-1,3))/float(ndiv(i-1))
           do j=1, ndiv(i-1)
              ii = ii + 1
              pk(ii,1) = pk(ii-1,1) + delx
              pk(ii,2) = pk(ii-1,2) + dely
              pk(ii,3) = pk(ii-1,3) + delz
           enddo
       enddo
c
      write(8,10)'k-points along high symmetry lines'
  10  format(A34)
      write(8,*)ntotkpt
      write(8,'(A10)')'Reciprocal' 
         weight=1.d0
         do i=1,ntotkpt
         write(7,200) pk(i,1),pk(i,2),pk(i,3),weight
         write(8,200) pk(i,1),pk(i,2),pk(i,3),weight
         enddo
 200  format(3F10.6,F6.2)
      stop
      end
c----------------------- end ---------------------------
