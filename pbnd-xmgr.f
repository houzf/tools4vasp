C     +--------------------------------------------------------
C     |  Purpose: Convert EIGENVAL for band plot using Origin6  
C     |  Program: pbnd.f 
C     |  Author:  Hou Zhufeng
C     |  Date:    10. 29. 2003
C     +--------------------------------------------------------
      program pbnd 
      real*8  eigup,eigdn,kp,xxx,xx
      integer  iii,nb,nk,nn,jj,i,j,ispin
      dimension eigup(500,500),eigdn(500,500),iii(3),xxx(5)
      real*8  eig
      dimension eig(500,500),kp(500,3)
      character*30  name
      real*8 xk1,yk1,zk1,xk2,yk2,zkz,a,b,x,y,z,disk,dish
      dimension a(3,3),b(3,3),disk(500),dish(10)
      character*2 labhk
      real*8 phighk,dlx,dd,efermi,emin,emax
      integer ndiv,ii,pskp
      dimension phighk(10,3)
      dimension labhk(10)
      dimension ndiv(10),pskp(10)
c
c---------read EIGENVAL
c
      open(4,file='EIGENVAL',status='old')
      read(4,*) (iii(i),i=1,3),ispin
      read(4,*) (xxx(i),i=1,5)
      read(4,*) xx
      read(4,*) name
      read(4,*) name
      read(4,*) nb,nk,nn
      if(ispin.eq.2) then
        open(7,file='BANDSUP')
        open(8,file='BANDSDN')
        do i=1,nk
          read(4,*) 
          read(4,*) (kp(i,j),j=1,3),w
          do  j=1,nn
             read(4,*) jj,eigup(i,j),eigdn(i,j)
          enddo
          write(7,100) i,(eigup(i,j),j=1,nn)
          write(8,100) i,(eigdn(i,j),j=1,nn)
        enddo
      else
        open(9,file='BANDS')
        do i=1,nk
          read(4,*) 
          read(4,*) (kp(i,j),j=1,3),w
          do j=1,nn
             read(4,*) jj,eig(i,j)
          enddo
          write(9,100) i,(eig(i,j),j=1,nn)
        enddo
      endif 
c---option:
100   format(i4,5x,80f10.4)
c 
c----- Read direct and reciprocal lattice vectors ------
c
      open(5,file='syml')
      read(5,*) nhighk
      read(5,*) (ndiv(i),i=1,nhighk-1)
      do i=1, nhighk
         read(5,*)  labhk(i),(phighk(i,j),j=1,3)
      enddo
      do i =1,3
         read(5,*) (a(i,j),j=1,3),(b(i,j),j=1,3)
c        read(5,*) (a(i,j),b(i,j),j=1,3)
      enddo
      read(5,*)emin, emax
      read(5,*)efermi
c
c---- Calculate disk(i) for band plot------
c
      disk(1)=0.d0
      dish(1)=0.d0
      dd = 0.d0
      ii=1
      do i=2, nhighk
       x = phighk(i-1,1)
       y = phighk(i-1,2)
       z = phighk(i-1,3)
       xk1 = x*b(1,1) + y*b(2,1) + z*b(3,1)
       yk1 = x*b(1,2) + y*b(2,2) + z*b(3,2)
       zk1 = x*b(1,3) + y*b(2,3) + z*b(3,3)
       x = phighk(i,1)
       y = phighk(i,2)
       z = phighk(i,3)
       xk2 = x*b(1,1) + y*b(2,1) + z*b(3,1)
       yk2 = x*b(1,2) + y*b(2,2) + z*b(3,2)
       zk2 = x*b(1,3) + y*b(2,3) + z*b(3,3)
       dish(i) = sqrt((xk2-xk1)**2+(yk2-yk1)**2+(zk2-zk1)**2)
cc
       do j=1,ndiv(i-1)
        ii= ii + 1
        dlx = dish(i)/float(ndiv(i-1))
        disk(ii) = ddd + dlx*float(j)
       enddo
        ddd = ddd + dish(i)
      enddo
c
c---- Write out data for  XMGRACE plot -----
c
      if(ispin.eq.2) then
        open(10,file='upbnd.dat')
        open(11,file='dnbnd.dat')
        do j = 1, nn
           do i=1,nk
            write(10,200) disk(i),eigup(i,j)-efermi
            write(11,200) disk(i),eigdn(i,j)-efermi
           enddo
           write(10,*)
           write(11,*)
        enddo
      else
        open(12,file='bnd.dat')
       do j = 1, nn
           do i=1,nk
            write(12,200) disk(i),eig(i,j)-efermi
          enddo
          write(12,*)
        enddo 
      endif 
c option---:
 200  format(f10.6,5x,f10.5)
      open(13,file='highk.dat')
      pskp(1)=1
      do i=2,nhighk
        pskp(i)=ndiv(i-1)+pskp(i-1)
      enddo
      do j =1,nhighk
         write(13,300)  disk(pskp(j)),emax
         write(13,300)  disk(pskp(j)),emin
         write(13,*)
      enddo
c option----
 300  format(f10.6,3x,f10.6)
c
      stop
      end
c----------------------- end ---------------------------
