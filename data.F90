SUBROUTINE collect(pa, data_mega, spec, age)
   USE the_info

   TYPE(agg_part), INTENT(IN) :: pa
   DOUBLE PRECISION, DIMENSION(15,seafloor/dz), INTENT(INOUT) :: data_mega
   DOUBLE PRECISION, DIMENSION(n_size,seafloor/dz), INTENT(INOUT) :: spec
   DOUBLE PRECISION, DIMENSION(10,seafloor/dz), INTENT(INOUT) :: age
   DOUBLE PRECISION :: organic 
   INTEGER :: i, j, l, aha

   ! find the depth at which the aggregates are
   CALL find_depth(pa%z, i)

   data_mega(2,i) = data_mega(2,i) + pa%orgC(1,1)*pa%n
   data_mega(3,i) = data_mega(3,i) + pa%orgC(2,1)*pa%n
   data_mega(4,i) = data_mega(4,i) + pa%orgC(3,1)*pa%n
   data_mega(5,i) = data_mega(5,i) + pa%orgC(4,1)*pa%n
   data_mega(6,i) = data_mega(6,i) + pa%orgC(5,1)*pa%n
   data_mega(7,i) = data_mega(7,i) + pa%orgC(6,1)*pa%n
   data_mega(8,i) = data_mega(8,i) + pa%orgC(7,1)*pa%n
   data_mega(9,i) = data_mega(9,i) + pa%orgC(8,1)*pa%n
   data_mega(10,i) = data_mega(10,i) + pa%orgC(9,1)*pa%n
   data_mega(11,i) = data_mega(11,i) + pa%orgC(10,1)*pa%n
   data_mega(12,i) = data_mega(12,i) + pa%mineral(1)*pa%n
   data_mega(13,i) = data_mega(13,i) + pa%mineral(2)*pa%n
   data_mega(14,i) = data_mega(14,i) + pa%mineral(3)*pa%n
   data_mega(15,i) = data_mega(15,i) + pa%mineral(4)*pa%n

   ! find the sizebin of the aggregates
   aha = 0
   l = 1
   DO WHILE (l .lt. n_size-1 .and. aha .eq. 0)
      IF (pa%r .lt. radi(l)) THEN
         aha = 1
      ELSE
         l = l + 1
      ENDIF
   ENDDO

!   spec(l,i) = spec(l,i) + pa%n
   organic = pa%orgC(1,1) + pa%orgC(2,1) + pa%orgC(3,1) + &
             pa%orgC(4,1) + pa%orgC(5,1) + pa%orgC(6,1) + &
             pa%orgC(7,1) + pa%orgC(8,1) + pa%orgC(9,1) + &
             pa%orgC(10,1) 

   spec(l,i) = spec(l,i) + pa%n


   if (spec(l+1,i) .gt. 1e18) THEN
      print*, 'spec:', spec(l,i), i
   endif
   if (l .eq. n_size) THEN
      print*, 'l eq n_size', pa%r, pa%s
   endif

   j = 2
   DO WHILE (j .le. 10) 
      age(j,i) = age(j,i) + pa%orgC(j-1,1)*pa%n
      j = j + 1
   ENDDO

END SUBROUTINE collect
!=========================================================================
SUBROUTINE reset(pa)
   USE the_info

   TYPE(agg_part), INTENT(INOUT) :: pa

   pa%b = 2
   pa%af = 0
   pa%r = 0
   pa%rho = 0 
   pa%por = 0
   pa%s = 0
   pa%z = 0
   pa%w = 0
   pa%n = 0
   pa%p = 0
   pa%Nn = 0
   pa%pr = 0
   pa%frac = 0
   pa%Th_234 = 0
   pa%Th_230 = 0
   i = 1
   DO WHILE (i .le. 10)
      pa%orgC(i,1) = 0
      pa%orgC(i,2) = 0
      pa%TEP(i,1) = 0
      pa%TEP(i,2) = 0
      i = i + 1
   ENDDO
   i = 1
   DO WHILE (i .le. 4) 
      pa%mineral(i) = 0
      i = i + 1
   ENDDO

END SUBROUTINE reset
!==========================================================================
SUBROUTINE calculate_flux(pa, data_flux)
   USE the_info

   TYPE(agg_part), INTENT(IN) :: pa
   DOUBLE PRECISION, DIMENSION(4), INTENT(INOUT) :: data_flux
   DOUBLE PRECISION :: organic, inorganic
   INTEGER :: i 
   
   i = 1
   organic = 0
   inorganic = 0
   DO WHILE (i .le. 10) 
     organic = organic + pa%orgC(i,1)*pa%n
     i = i + 1
   ENDDO
   inorganic = (pa%mineral(1)+pa%mineral(2))*pa%n

   data_flux(1) = data_flux(1) + organic*pa%w/dz*timestep
   data_flux(2) = data_flux(2) + inorganic*pa%w/dz*timestep
   data_flux(3) = data_flux(3) + pa%mineral(3)*pa%w/dz*timestep*pa%n
   data_flux(4) = data_flux(4) + pa%mineral(4)*pa%w/dz*timestep*pa%n

END SUBROUTINE calculate_flux
!==========================================================================
SUBROUTINE collect2(pa, data_mega)
   USE the_info

   TYPE(agg_part), INTENT(IN) :: pa
   DOUBLE PRECISION, DIMENSION(n_size,seafloor/dz,2,12), INTENT(INOUT) :: data_mega
   DOUBLE PRECISION :: organic, TEP, calc, rainratio
   INTEGER :: max_i, max_l
   INTEGER :: i, j, l, aha, x, y

   aha = 0
   ! find the depth at which the aggregates are
   CALL find_depth(pa%z, i)

   ! find the sizebin of the aggregates
   aha = 0
   l = 1
   DO WHILE (l .lt. n_size-1 .and. aha .eq. 0)
      IF (pa%r .lt. radi(l)) THEN
         aha = 1
      ELSE
         l = l + 1
      ENDIF
   ENDDO

   m = 1
   organic = 0
   TEP = 0
   DO WHILE (m .le. 10) 
     organic = organic + pa%orgC(m,1)*pa%n
     TEP = TEP + pa%TEP(m,1)*pa%n
     m = m + 1
   ENDDO
   
   calc = pa%mineral(1)*pa%n + pa%mineral(2)*pa%n
   IF (calc .gt. 0) THEN
      rainratio = organic/calc
   ENDIF

   ! check if the indexes are within the array
#ifdef array_size
   max_i = UBOUND(data_mega,2)
   max_l = UBOUND(data_mega,1)
   CALL check_index(i, max_i, x)
   CALL check_index(l, max_l, y)
   IF (x .gt. 0 .or. y .gt. 0) THEN
      print*, 'OVERWRITE ARRAY in collect2'
      print*, i, max_i, l, max_l
      print*, pa%id, pa%z, pa%r
   ENDIF
#endif

   data_mega(l,i,1,1) = data_mega(l,i,1,1) + pa%rho*pa%n
   data_mega(l,i,2,1) = data_mega(l,i,2,1) + pa%n
   data_mega(l,i,1,2) = data_mega(l,i,1,2) + pa%w*pa%n
   data_mega(l,i,2,2) = data_mega(l,i,2,2) + pa%n
   data_mega(l,i,1,3) = data_mega(l,i,1,3) + pa%s*pa%n
   data_mega(l,i,2,3) = data_mega(l,i,2,3) + pa%n
   data_mega(l,i,1,4) = data_mega(l,i,1,4) + organic
   data_mega(l,i,1,5) = data_mega(l,i,1,5) + calc
   data_mega(l,i,1,6) = data_mega(l,i,1,6) + pa%mineral(3)*pa%n
   data_mega(l,i,1,7) = data_mega(l,i,1,7) + pa%mineral(4)*pa%n
   data_mega(l,i,1,8) = data_mega(l,i,1,8) + rainratio * pa%n
   data_mega(l,i,2,8) = data_mega(l,i,2,8) + pa%n
   data_mega(l,i,1,9) = data_mega(l,i,1,9) + pa%por*pa%n
   data_mega(l,i,2,9) = data_mega(l,i,2,9) + pa%n
   data_mega(l,i,1,10) = data_mega(l,i,1,10) + pa%Th_234*pa%n
   data_mega(l,i,2,10) = data_mega(l,i,2,10) + pa%n
   data_mega(l,i,1,11) = data_mega(l,i,1,11) + pa%Th_230*pa%n
   data_mega(l,i,2,11) = data_mega(l,i,2,11) + pa%n
   data_mega(l,i,1,12) = data_mega(l,i,1,12) + TEP

END SUBROUTINE collect2
!=========================================================================
SUBROUTINE sediment_trap(p, z)
! this subroutine collects the flux that passes a certain depth
   USE the_info

   TYPE(agg_part), INTENT(IN) :: p
   DOUBLE PRECISION :: TEP
   INTEGER, INTENT(IN) :: z
   INTEGER :: i

   i = 1
   TEP = 0
   DO WHILE (i .le. 10)
      sedTrap(z,i) = sedTrap(z,i) + p%orgC(i,1)*p%n 
      TEP = TEP + p%TEP(i,1)
      i = i + 1
   ENDDO

   sedTrap(z,11) = sedTrap(z,11) + p%mineral(1)*p%n + p%mineral(2)*p%n
   sedTrap(z,12) = sedTrap(z,12) + p%mineral(3)*p%n
   sedTrap(z,13) = sedTrap(z,13) + p%mineral(4)*p%n
   sedTrap(z,14) = sedTrap(z,14) + TEP*p%n
   
   IF (p%mineral(1) .lt. 0 .or. p%mineral(3) .lt. 0) THEN
      print*, 'sedTrap:', p%mineral(1), p%mineral(2)
   ENDIF
   IF (sedTrap(z,11) .lt. 0) print*, 'sedTrap problem', &
      p%Nn, p%n, p%id

END SUBROUTINE sediment_trap
!=========================================================================
