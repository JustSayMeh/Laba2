      program main
      implicit none
      integer max
      parameter (max = 100000)
      real xl(max), yl(max)
      real xmin, xmax, xs, ymin, ymax, ys, pi, zr, ninf, pinf
      common /inputs/xmin,xmax,xs,ymin,ymax,ys
      common /consts/pi
      common /infinites/ pinf, ninf
      zr = 0.0
      pinf = -log(zr)
      ninf = -pinf
      pi = asin(1.0) * 2.0
      call input
      call processF(xl(1),yl(1))
      call output(xl(1), yl(1))
      print *,'Sucsessful'
      end program main

      integer function feq(x)
      implicit none
      real x, y, p, a, b
      ! 90.0 + x = (1 + x/90.0) * 90.0 = 90.0
      if (abs(abs(x) - 90.0).lt.0.005) then
      feq = 1
      else
      feq = 0
      end if
      end function feq



      real function f(x, y)
      implicit none
      character(12) buf, buf2
      integer okl, feq
      real x, y, hjk, hjk2
      real xmin,xmax,xs,ymin,ymax,ys,pi, pinf, ninf, bf, rbf
      common /inputs/xmin,xmax,xs,ymin,ymax,ys
      common /consts/pi
      common /infinites/ pinf, ninf
      bf = x + y
      if (abs(bf).gt.180.0) then
      okl = INT(bf / 180.0)
      bf = bf - okl * 180.0
      end if
      rbf = bf * pi / 180.0
      !write(buf, "(E10.4)" ) bf
      !write(buf2, "(E10.4)" ) 0.0
      hjk = max(abs(x), abs(y))
      if (feq(bf).eq.1) then
      f = 0.0
      else if (hjk.eq.0.0.or.(abs(x + y) / hjk).lt.0.00005) then
      f = pinf
      else
      f = cos(rbf)/sin(rbf)
      end if
      end function f

      subroutine input
      implicit none
      real xmin, xmax, xs, ymin, ymax, ys, pi
      common /inputs/xmin, xmax, xs, ymin, ymax, ys
      common /consts/pi
      open (1, file='input.txt', status='old', err=1)
      read (1,*, end=2), xmin, xmax, xs, ymin, ymax, ys
      goto 3
 1    print *, 'Error: File is missing'
      stop
 2    print *, 'Error: Variables is missing'
      close (1)
      stop
 3    end subroutine input

      subroutine processF(xl, yl)
      implicit none
      integer max
      parameter (max = 100000)
      real xl(max), yl(max)
      real xmin, xmax, xs, ymin, ymax, ys, vxs, vys, pi
      integer n, m, stepFilter
      common /inputs/xmin,xmax,xs,ymin,ymax,ys
      common /calcvar/n, m
      common /minsteps/ vxs, vys
      common /consts/pi
      n = INT((xmax - xmin) / xs + 1)
      m = INT((ymax - ymin) / ys + 1)
      xl(1) = xmin
      yl(1) = ymin
      if (n.le.0.or.m.le.0) goto 4
      n = stepFilter(xl, n, xmin, xs)
      m = stepFilter(yl, m, ymin, ys)
      !xl(n) = xmax
      !yl(m) = ymax
      goto 5
4     print *, 'Error: Intervals are not defined correctly'
      stop
6     print *, 'Error: Unsuccessful step'
      stop
5     end subroutine processF




      integer function stepFilter(l, n, mn, sp)
      implicit none
      logical b
      integer k, n, i, swch, swch2
      real th, mn, sp, l(n)
      character (24) buf
      b = .TRUE.
      k = 1
      swch = 12*mod(k,2)
      write (buf(13: 24), "(E10.4)") l(1)
      do i=1,n-1
      swch = 12*mod(k,2)
      swch2 = 12 - swch
      th = mn + i * sp
      write (buf(1 + swch2: 12 + swch2), "(E10.4)") th
      if (buf(1 + swch2: 12 + swch2).eq.buf(1 + swch: 12 + swch))
     *goto 998
      !print *, th, sp, th / mn
      k = k + 1
      !mn - переменная задаваеммая пользователем. 0.0 - переводится свободно
      if (mn.ne.0.0.and.b.and.abs((th) / mn).lt.1e-7) then
      l(k) = 0.0
      b = .FALSE.
      else
      l(k) = th
      end if
998   end do
      stepFilter = k
      end function stepFilter


      subroutine output(xl, yl)
      implicit none
      character(45) FMT, FMTHEAD
      character(45) DELM
      integer max, n, m, i, j
      parameter (max = 100000)
      real xl(max), yl(max)
      real xmin, xmax, xs, ymin, ymax, ys, pi, f
      common /inputs/xmin,xmax,xs,ymin,ymax,ys
      common /calcvar/n, m
      common /consts/pi
      open (2, file='output.txt')
      write(DELM, 900) (n+1) * 13
      write(FMT, 901), '" || "',n,'," | "'
      write(FMTHEAD, 902),'" Y   \   X"1x,"|| "', n, '," | "'
      write (2,FMTHEAD)((xl(j)),j=1,n)
      write (2, DELM) ('=', i = 1,(n+1) * 13)
      do i=1,m
      write (2,FMT) (yl(i)),(f(yl(i),xl(j)),
     *j = 1, n)
      end do
      close (2)
 900  format ("(",I0,"A",")")
 901  format ("(","E10.4",",",A,",",I0,"(","E10.4",A,")",")")
 902  format ("(",A,I0,"(","E10.4",A,")",")")
      end subroutine output


      !if (x+y.lt.pi/2.0+e.and.x+y.gt.pi/2.0-e) then
      !f = 0.0
      !else if (abs(x+y).lt.e.or.(x+y.lt.pi+e.and.x+y.gt.pi-e))then
      !f = pinf
      !else
      !f = cos(x + y)/sin(x + y)

