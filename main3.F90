#define respi
#define dissol
#define microzoo
#define coag2
#define agin
#define sinking
#define dissolved
!#define thorium
#define disintegr
#define housekeeping
#define selfCollision
#define photo
#define writing
#define array_size

MODULE the_info
   IMPLICIT NONE
   SAVE

   TYPE agg_part  !Here are all the attributes of the particle (aggregate)
      INTEGER :: id, b, af !identification number, bottom, agg or fecal pell
! b=0: in watercolumn, b=1: on bottom, b=2: has been reset
! af=0: aggregate, af=1: fecal pellet
      DOUBLE PRECISION :: r, rho, por, s, z, w, n, p, Nn 
      DOUBLE PRECISION :: pr, frac, Th_234, Th_230
      DOUBLE PRECISION, DIMENSION(4) :: mineral
      DOUBLE PRECISION, DIMENSION(10,2) :: orgC, TEP
      !radius, density, porosity, stickiness, depth, settl. vel
      ![um,g/cm3,-,-,cm, cm/s, #,#_total,#/n
      ! n = number of aggregates this aggregate represents in m3
      ! p = number of primary particles in all the aggregages
      ! Nn = number of primary particles in each aggregate
      ! orgC : [molC/aggregate]
      ! minerals(calc, arag, opal, clay) 
      ! Th [molTh/particle]
   END TYPE
   TYPE zooplankton  !This is to keep track of the zooplankton
      DOUBLE PRECISION :: P, Z
      ! P and Z in molC/m3, P=phytoplankton, Z=zooplankton
   END TYPE 

!This is to keep track of orgC, CaCO3, etc inventory
   DOUBLE PRECISION, DIMENSION(4) :: inventory = [0,0,0,0]
   DOUBLE PRECISION, DIMENSION(5) :: sf = [0,0,0,0,0] !seafloor
   DOUBLE PRECISION, DIMENSION(5) :: npp = [0,0,0,0,0] !net primary prod
   DOUBLE PRECISION, DIMENSION(5) :: lost = [0,0,0,0,0]   ! stuff lost in wc: org,calc,opal,clay,TEP
   DOUBLE PRECISION :: Th_inv  ! to keep track of Th
   DOUBLE PRECISION, DIMENSION(26,14) :: sedTrap
   DOUBLE PRECISION, DIMENSION(19) :: seaBed

! timestep should not be greater than 1 day (eg. disintegrate)
   INTEGER, PARAMETER :: timestep = 3600*8   !s
   INTEGER, PARAMETER :: endoftime = 1095*18 !number of timesteps in run
   INTEGER, PARAMETER :: seafloor = 400000    !cm
   INTEGER, PARAMETER :: dz = 1000         !cm
   INTEGER, PARAMETER :: n_size = 60
   DOUBLE PRECISION, PARAMETER :: a_number=200000

! The following are to view results
! how much orgC and CaCO3 is consumed by bact and zoop at each depth 
   DOUBLE PRECISION,DIMENSION(seafloor/dz) :: orgC_G1, orgC_b, opal_t
   DOUBLE PRECISION,DIMENSION(seafloor/dz) :: calcC_t, calcA_t, calc_G1
   DOUBLE PRECISION,DIMENSION(seafloor/dz) :: Th_d_234, Th_d_230    ![dpm/l]
   DOUBLE PRECISION, DIMENSION(n_size) :: radi
! To track the size, velocity, density of the particles that reach the seafloor
   DOUBLE PRECISION, DIMENSION(n_size) :: sizeBed, veloBed, densBed
   DOUBLE PRECISION, DIMENSION(2,n_size) :: scaling

! physical constants
   DOUBLE PRECISION, PARAMETER :: k = 1.38d-23
   DOUBLE PRECISION, PARAMETER :: T = 285d00 !K
   DOUBLE PRECISION, PARAMETER :: mu = 8.9d-4, gamma = 0.01
   DOUBLE PRECISION, PARAMETER :: pi = 3.14159265358979323846d00
   DOUBLE PRECISION, PARAMETER :: rho_sw = 1.02695d00
   DOUBLE PRECISION, PARAMETER :: rho_orgC = 1.06d00, mw_orgC = 28.8d00
   DOUBLE PRECISION, PARAMETER :: rho_co = 2.8275d00, mw_co = 100.1d00
   DOUBLE PRECISION, PARAMETER :: rho_si = 1.65d00, mw_si = 67.3d00
   DOUBLE PRECISION, PARAMETER :: rho_li = 2.65d00, mw_li = 60d00
   DOUBLE PRECISION, PARAMETER :: rho_TEP = 0.8, mw_TEP = 154.8d00 ! 154.8
   DOUBLE PRECISION, PARAMETER :: g = 981 !cm/s
   DOUBLE PRECISION, PARAMETER :: k_caco3_c = 5.0/86400
   DOUBLE PRECISION, PARAMETER :: k_caco3_a = 3.2/86400
   DOUBLE PRECISION, PARAMETER :: eta_c = 4.5
   DOUBLE PRECISION, PARAMETER :: eta_a = 4.2
   DOUBLE PRECISION, PARAMETER :: A_U_238 = 2.8, Th_km1 = 6.8e-3/86400 ![1/s], Clegg & Whitfield, 1993, units A_U [dpm/L]
   DOUBLE PRECISION, PARAMETER :: A_U_234 = A_U_238*5.472d-6!5 
   DOUBLE PRECISION, PARAMETER :: lambda_Th_234 = 2.876d-2/86400 ![1/d*d/s = 1/s]
   DOUBLE PRECISION, PARAMETER :: lambda_Th_230 = 2.525d-8/86400 ![1/d*d/s = 1/s]

END MODULE the_info
!====================================================================

PROGRAM main

   USE the_info
   IMPLICIT NONE

   101 FORMAT (E, 1X, E, 1X, I5, E, 1X, E)
   102 FORMAT (E15.4E2, 1X, E15.4E2, 1X, I)
   103 FORMAT (E15.4E2, 1X, I5, 1X, E15.4E2)
   104 FORMAT (E15.4E2, 1X, E15.4E2, 1X, E15.4E2)
   110 FORMAT (I4)
   111 FORMAT (E15.4E2)
   118 FORMAT (5(F12.6, 1X))
   119 FORMAT (10F18.6)
   120 FORMAT (E15.6E2, 3X, F14.4, 1X, I)
   125 FORMAT (E15.6E2, 3X, F14.4, 1X, I, 1X, I)
   121 FORMAT (E, 1X, F)
   122 FORMAT (2(E))
   145 FORMAT (E15.4E2, 1X, E15.4E2, 1X, E15.4E2, 1X, I4)
   146 FORMAT (F, 1X, 15(E15.6E2, 1X))
   147 FORMAT (5(F,1X),13(E14.4E3, 1X))
   200 FORMAT (13E, 1X, I5, 1X, I5, 1X, 3E)
   220 FORMAT (I5, 1X, 4E, 1X, I5)
   240 FORMAT (5E)
   245 FORMAT (6E)
   250 FORMAT (I, 1X, 4E)
   260 FORMAT (I5, 1X, 6E)
   
   INTEGER :: ka, time, i, j, l, agg_max, z_max, temp1, temp2, tmp1, tmp2
   INTEGER :: intT, intC, intD, intP, intS, intTEP, intTEP2
   INTEGER :: intFrac, yaff, bal1, bal2, bal3, resol
   INTEGER :: UNIT, free_count, loop, aha
   INTEGER, DIMENSION(1) :: old, seed
   INTEGER, DIMENSION(a_number), TARGET :: agg_z
   INTEGER, DIMENSION(10), TARGET :: agg_pool
   INTEGER, DIMENSION(a_number) :: free_agg
   INTEGER, POINTER :: pz
   DOUBLE PRECISION, DIMENSION(1) :: harvest
   DOUBLE PRECISION, DIMENSION(seafloor/dz,14) :: flux
   DOUBLE PRECISION, DIMENSION(seafloor/dz/5,14) :: fluxAve
   DOUBLE PRECISION :: depth, temp, dCO3c, dCO3a, max_r, organic, dummy
   DOUBLE PRECISION :: paraT, paraC, paraD, paraP, paraS, track_1, track_2
   DOUBLE PRECISION :: paraTEP, paraStick, paraFrac, track_3, fudge
   DOUBLE PRECISION :: orgC1, orgC4, caco31, caco34, littleFood, bigFood, food
   DOUBLE PRECISION :: Fsize, balC, balO, balD, resolution
   DOUBLE PRECISION :: Th_total, before, after, TEP_frac, year, season
   DOUBLE PRECISION :: mass, day, track4, track5, track6, track7
   DOUBLE PRECISION, DIMENSION(seafloor/dz) :: orgCtotal, calctotal, rainratio
   DOUBLE PRECISION, DIMENSION(seafloor/dz) :: orgCMean, calcMean, rainMean
   DOUBLE PRECISION, DIMENSION(seafloor/dz) :: opalMean, clayMean, Th_sum_234
   DOUBLE PRECISION, DIMENSION(seafloor/dz) :: Th_sum_230, mass_sum
   DOUBLE PRECISION, DIMENSION(5) :: mass_i, mass_f
   DOUBLE PRECISION, DIMENSION(15,seafloor/dz) :: data_stuff
   DOUBLE PRECISION, DIMENSION(n_size,seafloor/dz) :: spec
   DOUBLE PRECISION, DIMENSION(10,seafloor/dz) :: age
   DOUBLE PRECISION, DIMENSION(n_size,seafloor/dz,2,12) :: data_mega
   DOUBLE PRECISION, DIMENSION(4) :: data_flux
   TYPE(agg_part), DIMENSION(a_number), TARGET :: agg
   TYPE(agg_part), POINTER :: pa, pa1, pa2
   TYPE(zooplankton), DIMENSION(seafloor/dz) :: mic
   CHARACTER (len=14) :: ctime, parP, parC, parT, parS
   CHARACTER (len=30) :: file_name, spec_name, age_name, martin_name

   READ(5,*) intT,intC,intD,intP,intS,intTEP,intTEP2,intFrac,yaff, &
             bal1, bal2, bal3!, resol
             
   
   paraT = intT    !SST
   paraC = intC    !CO3=
   paraD = intD    !zoopl. dissol.
   paraP = intP    !primary prod.
   paraS = intS    !seasonality
   paraTEP = intTEP!fraction of pp that is TEP
   paraStick = intTEP2!stickiness of TEP as it degrades
   paraFrac = intFrac/10.0 !fractal dimension of large particles
   fudge = yaff/100.0  !This is a knob I use for different sensitivity studies
   balC = bal1/100.0   !How much CaCO3 there is in a coccolith
   balO = bal2/10.0    !How much Si in a diatom
   balD = bal3/10.0    !How much dust

#ifdef writing
   OPEN (UNIT=12, FILE="orgC", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=13, FILE="calc", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=14, FILE="opal", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=15, FILE="clay", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=150, FILE="rratio", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=18, FILE="seafloor", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=19, FILE="primaryProd", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=20, FILE="density.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=21, FILE="velocity.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=22, FILE="stickiness.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=23, FILE="organic.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=24, FILE="inorganic.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=25, FILE="opal.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=26, FILE="clay.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=27, FILE="rainratio.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=28, FILE="orgFloor", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=29, FILE="mineralFloor", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=30, FILE="pelletFloor", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=31, FILE="ratioFloor", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=33, FILE="orgFlux1m", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=34, FILE="orgFlux1km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=35, FILE="orgFlux4km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=36, FILE="calcFlux1m", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=37, FILE="calcFlux1km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=38, FILE="calcFlux4km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=39, FILE="opalFlux1m", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=40, FILE="opalFlux1km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=41, FILE="opalFlux4km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=42, FILE="ratioFlux1m", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=43, FILE="ratioFlux1km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=44, FILE="ratioFlux4km", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=45, FILE="fluxSeafloor", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=46, FILE="porosity.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=47, FILE="thorium234.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=48, FILE="one.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=49, FILE="two.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=50, FILE="three.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=51, FILE="four.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=52, FILE="seafloorOrg.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=54, FILE="thorium230.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=55, FILE="TEP.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=100, FILE="logMic.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=101, FILE="logMac.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=104, FILE="seaFloor.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=105, FILE="logMic2.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=106, FILE="logMic4.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=110, FILE="seafloor1yr", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=111, FILE="veloFloor.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=112, FILE="densFloor.out", STATUS='NEW', POSITION='APPEND')
   OPEN (UNIT=113, FILE="scaling.out", STATUS='NEW', POSITION='APPEND')
#endif

! random numbers
   ka = 1
   old = 1
   seed(1) = 1834
   CALL random_seed
   CALL random_seed(size=ka)
   CALL random_seed(GET=old(1:ka))
   CALL random_number(harvest(1))

! write the radius vector
   aha = 0 
   i = 1
   max_r = 1d00
   DO WHILE (i .le. n_size .and. aha .eq. 0) 
      radi(i) = max_r
      max_r = max_r * 1.3d00
      i = i + 1
   ENDDO 
#ifdef thorium
! initialize the dissolved thorium vector
   Th_d_234(:) = 2.4      !dpm/L
   Th_d_230(:) = 2.4e-6      !dpm/L
#endif thorium

   time = 1
   day = 365
   agg_max = 1 !the next available spot
   free_count = 1
   free_agg = 0

   sedTrap(:,:) = 0

   mic(:)%P = 0.0
   mic(:)%Z = 0.0

   mass_i(:) = 0.0

! Fraction of organic carbon that is TEP
   TEP_frac = paraTEP/100.0 !0.2

   year = 0

   DO WHILE (time .le. endoftime) 
      i = 1
      loop = FLOOR(20*1095/paraS) 
      organic = 0
      IF (time .eq. 1095*(year+1)) THEN
         year = year + 1
      ENDIF
      IF (time-year*1095 .lt. paraS) THEN
         season = paraP/12.0e3 * 1095/paraS   !prod per day [molC/d]
      ELSE
         season = 0
      ENDIF
! primary production 
      DO WHILE (i .le. loop .and. season .gt. 0)
         IF (free_count .gt. 1) THEN
            pa => agg(free_agg(free_count-1))
            pa%id = free_agg(free_count-1)
            free_count = free_count-1
         ELSEIF (free_count .eq. 1) THEN
            pa => agg(agg_max)
            pa%id = agg_max
            agg_max = agg_max + 1
         ENDIF
         IF (pa%id .gt. a_number) print*, 'END OF LIST', pa%id
         CALL random_number(harvest)
         CALL production(pa, harvest, loop, time, season, paraT, paraFrac,&
                         balC, balO, balD)
         mass_i(1) = mass_i(1) + pa%orgC(1,1)*pa%n
         mass_i(2) = mass_i(2) + (pa%mineral(1)+pa%mineral(2))*pa%n
         mass_i(3) = mass_i(3) + pa%mineral(3)*pa%n
         mass_i(4) = mass_i(4) + pa%mineral(4)*pa%n
         organic = organic + pa%orgC(1,1)*pa%n
         i = i + 1
      ENDDO 
! TEP production
      IF (season .gt. 0) THEN
         IF (free_count .gt. 1) THEN
            pa => agg(free_agg(free_count-1))
            pa%id = free_agg(free_count-1)
            free_count = free_count-1
         ELSEIF (free_count .eq. 1) THEN
            pa => agg(agg_max)
            pa%id = agg_max
            agg_max = agg_max + 1
         ENDIF
         CALL random_number(harvest)
         CALL TEP_prod(pa,organic,harvest, TEP_frac, paraT, paraFrac)
         mass_i(5) = mass_i(5) + pa%TEP(1,1)*pa%n
      ENDIF
! keeping track of how much orgC is lost to protista, zoop and bacteria
      i = 1
      track_1 = 0
      track_2 = 0
      track_3 = 0
      track4 = 0
      track5 = 0
      track6 = 0
      track7 = 0
      DO WHILE (i .le. seafloor/dz)
         track_1 = track_1 + orgC_G1(i)
         track_3 = track_3 + orgC_b(i)
         track4 = track4 + calc_G1(i)
         track6 = track6 + calcC_t(i)
         track7 = track7 + calcA_t(i)
         i = i + 1
      ENDDO

! find agg from same depth and put them into an array
      depth = 0
      DO WHILE (depth .lt. seafloor) 
         z_max = 0
         agg_z = 0
         i = 1
         j = 1
         DO WHILE(i .lt. agg_max) 
            IF (agg(i)%b .eq. 0) THEN
               pz => agg_z(j)
               pa => agg(i)
               CALL check_depth(pa, pz, depth, i, j, z_max) 
            ENDIF
            i = i + 1
         ENDDO 
! make aggregates at similar depth aggregate
         i = 1
         j = 1
         DO WHILE (i .le. z_max) 
            DO WHILE (j .le. z_max) 
! randomly find two aggregates in one depth box
  CALL random_number(harvest)
  tmp1 = agg_z(FLOOR(harvest(1) * z_max))
  IF (FLOOR(harvest(1)*z_max) .eq. 0) tmp1 = agg_z(z_max)

  CALL random_number(harvest)
  tmp2 = agg_z(FLOOR(harvest(1) * z_max))
  IF (FLOOR(harvest(1)*z_max) .eq. 0) tmp2 = agg_z(z_max)

  pa1 => agg(tmp1)
  pa2 => agg(tmp2)

!This is where it is decided if the two randomly picked particles above
!do collide
               IF (pa1%b .eq. 0 .and. pa2%b .eq. 0) THEN
!                  IF (i .eq. j) THEN
                  IF (tmp1 .eq. tmp2) THEN
#ifdef selfColl 
                     CALL self_collide(pa1, harvest, paraStick, paraFrac)
#endif
!                  ELSEIF (i .ne. j) THEN
                  ELSEIF (tmp1 .ne. tmp2) THEN
#ifdef coag2
                        CALL collide4(pa1, pa2, harvest, paraStick, paraFrac,paraT)
#endif
                  ENDIF
               ENDIF
               j = j + 1
            ENDDO 
            i = i + 1
            j = i + 1
         ENDDO 

! make zooplankton array using the agg_z() created above
         i = 1
         food = 0
         DO WHILE (i .le. z_max) 
            temp1 = agg_z(i)
            pa1 => agg(temp1)
            IF (pa1%af .eq. 0) THEN
               j = 1
               DO WHILE (j .le. 5) 
                  food = food + pa1%orgC(j,1)*pa1%n
                  j = j + 1
               ENDDO 
            ENDIF
            i = i + 1
         ENDDO 

         CALL find_depth(depth, i)

         food = food * 1/(dz/100.0)
         mic(i)%P = food                 !molC/m3
         IF (food .gt. 0) THEN
            CALL protists(mic(i),i, paraT,fudge)
         ENDIF

#ifdef thorium
! Calculate sources minus sinks for Th
         CALL find_depth(depth, i)
         CALL radioactive_decay(Th_d_234(i), Th_d_230(i))
#endif
#ifdef housekeeping
         i = 1
         j = 1
         agg_pool = 0
         DO WHILE (i .le. z_max) 
            temp1 = agg_z(i)
            pa1 => agg(temp1)
            IF (pa1%Nn .eq. 1 .and. pa1%s .lt. 0.01 .and. &
                pa1%mineral(4) .lt. 2.8d-13) THEN
              agg_pool(j) = temp1
              j = j + 1
            ENDIF
            i = i + 1
         ENDDO 
         IF (j .gt. 3) THEN
            i = j - 2
            DO WHILE (i .ge. 1) 
               temp1 = agg_pool(j-2)
               temp2 = agg_pool(j-1)
               CALL pool(agg(temp1), agg(temp2))
               i = i - 2
               j = j - 2
            ENDDO 
         ENDIF
#endif
         depth = depth + dz
      ENDDO 
#ifdef writing
! write out the zooplankton array
         IF (MOD(time,1000) .eq. 0) THEN 
            WRITE(ctime,*) time
            ctime = adjustl(ctime)
            WRITE(file_name,"(a30)")(trim(ctime)//".zoo")
            OPEN(UNIT=56, FILE=file_name, STATUS='NEW', POSITION='APPEND')
            i = 1
            DO WHILE (i .le. seafloor/dz) 
               WRITE(56,101) mic(i)%P, mic(i)%Z, i
               i = i + 1
            ENDDO
         ENDIF
#endif
         IF (MOD(time,90) .eq. 0) THEN
print*, '---------------------------', agg_max, '--------'
print*, 'npp:', npp, 'sf:', sf, 'time:', time
print*, '-----------------*-----------------'
            WRITE(110,119) sf(1), sf(2), sf(3), sf(4), sf(5), npp(1), &
                           npp(2), npp(3), npp(4), npp(5)
            sf(:) = 0
            npp(:) = 0
         ENDIF
         IF (MOD(time,1095) .eq. 0) THEN
! calculate the flux array
print*, 'calculating flux', time
            flux(:,:) = 0
            fluxAve(:,:) = 0
            i = 1
            DO WHILE (i .lt. agg_max)
               pa1 => agg(i)
               CALL find_depth(pa1%z, l)
               j = 1
               DO WHILE (j .le. 10) 
                  flux(l,j) = flux(l,j)+pa1%orgC(j,1)*pa1%n*pa1%w/dz*timestep*3
                  j = j + 1
               ENDDO 
               flux(l,11) = flux(l,11)+pa1%mineral(1)*pa1%n*pa1%w/dz*timestep*3
               flux(l,12) = flux(l,12)+pa1%mineral(2)*pa1%n*pa1%w/dz*timestep*3
               flux(l,13) = flux(l,13)+pa1%mineral(3)*pa1%n*pa1%w/dz*timestep*3
               flux(l,14) = flux(l,14)+pa1%mineral(4)*pa1%n*pa1%w/dz*timestep*3
               i = i + 1
            ENDDO 
            ! average over each 50m interval
            i = 1
            j = 1
            l = 1
            DO WHILE (l .le. seafloor/dz/10)
               DO WHILE (i .le. l*10)
                  DO WHILE (j .le. 14)
                     fluxAve(l,j) = fluxAve(l,j)+flux(i,j)
                     j = j + 1 
                  ENDDO
                  i = i + 1
                  j = 1
               ENDDO
               i = l*10
               l = l + 1
               j = 1
            ENDDO

            l = 1
            j = 1
            DO WHILE (l .le. seafloor/dz/10)
               DO WHILE (j .le. 14)
                  fluxAve(l,j) = fluxAve(l,j)/10.0
                  j = j + 1
               ENDDO
               j = 1
               l = l + 1
            ENDDO
! write out the calculated flux 
   ! parameters for file-names
            WRITE(ctime,*) time
            ctime = adjustl(ctime)
            WRITE(parT,*) intT
            parT = adjustl(parT)
            WRITE(parC,*) intC
            parC = adjustl(parC)
            WRITE(parS,*) intS
            parS = adjustl(parS)
            WRITE(parP,*) intTEP
            parP = adjustl(parP)
   ! open the files to write flux into 
!OPEN(UNIT=55, FILE=file_name, STATUS='NEW', POSITION='APPEND')
!OPEN(UNIT=102, FILE=file_name, STATUS='NEW', POSITION='APPEND')

!            WRITE(file_name,"(a30)")(trim(ctime)//trim(parT)//trim(parC)&
!                                    //trim(ParS)//trim(parP)//".mar")
!            OPEN(UNIT=55, FILE=file_name, STATUS='NEW', POSITION='APPEND')
!
            WRITE(file_name,"(a30)")(trim(ctime)//".ave")!//trim(parT)//trim(parC)&
            OPEN(UNIT=102, FILE=file_name, STATUS='NEW', POSITION='APPEND')

            WRITE(file_name,"(a14)")("trap"//trim(ctime))!//trim(parP)//trim(parC))
            OPEN(UNIT=53, FILE=file_name, STATUS='NEW', POSITION='APPEND')
   ! we average over a year, and return the flux in [molC/m2d]
            i = 1
            DO WHILE (i .le. seafloor/dz) 
               WRITE(55,200) flux(i,1), flux(i,2), flux(i,3),&
               flux(i,4), flux(i,5), flux(i,6), flux(i,7),&
               flux(i,8), flux(i,9), flux(i,10), flux(i,11),&
               flux(i,12), flux(i,13), time*timestep/86400, i*dz/100, &
               (flux(i,1)+flux(i,2)+flux(i,3)+flux(i,4)+flux(i,5)+&
               flux(i,6)+flux(i,7)+flux(i,8)+flux(i,9)+flux(i,10))
               i = i + 1
            ENDDO  
            flux(:,:) = 0
    ! write the 100m average flux 
            i = 1
            DO WHILE (i .le. seafloor/dz/10) 
               WRITE(102,200) fluxAve(i,1), fluxAve(i,2), fluxAve(i,3),&
               fluxAve(i,4), fluxAve(i,5), fluxAve(i,6), fluxAve(i,7),&
               fluxAve(i,8), fluxAve(i,9), fluxAve(i,10), fluxAve(i,11),&
               fluxAve(i,12), fluxAve(i,13), time*timestep/86400, i*100-50, &
               (fluxAve(i,1)+fluxAve(i,2)+fluxAve(i,3)+fluxAve(i,4)+fluxAve(i,5)+&
               fluxAve(i,6)+fluxAve(i,7)+fluxAve(i,8)+fluxAve(i,9)+fluxAve(i,10))
               i = i + 1
            ENDDO  
            fluxAve(:,:) = 0


   ! "measured" sediment traps
   ! we average over a year, and return the flux in [molC/m2d]
            i = 1
            DO WHILE (i .le. 9) 
               WRITE(53,200) sedTrap(i,1)/day, sedTrap(i,2)/day, sedTrap(i,3)/day,&
               sedTrap(i,4)/day, sedTrap(i,5)/day, sedTrap(i,6)/day, sedTrap(i,7)/day,&
               sedTrap(i,8)/day, sedTrap(i,9)/day, sedTrap(i,10)/day, sedTrap(i,11)/day,&
               sedTrap(i,12)/day, sedTrap(i,13)/day, time*timestep/86400, i*10, &
               (sedTrap(i,1)+sedTrap(i,2)+sedTrap(i,3)+sedTrap(i,4)+sedTrap(i,5)+&
               sedTrap(i,6)+sedTrap(i,7)+sedTrap(i,8)+sedTrap(i,9)+sedTrap(i,10))/day, &
               sedTrap(i,14)/day
               i = i + 1
            ENDDO
            DO WHILE (i .le. 20) 
               WRITE(53,200) sedTrap(i,1)/day, sedTrap(i,2)/day, sedTrap(i,3)/day,&
               sedTrap(i,4)/day, sedTrap(i,5)/day, sedTrap(i,6)/day, sedTrap(i,7)/day,&
               sedTrap(i,8)/day, sedTrap(i,9)/day, sedTrap(i,10)/day, sedTrap(i,11)/day,&
               sedTrap(i,12)/day, sedTrap(i,13)/day, time*timestep/86400, (i-9)*100, &
               (sedTrap(i,1)+sedTrap(i,2)+sedTrap(i,3)+sedTrap(i,4)+sedTrap(i,5)+&
               sedTrap(i,6)+sedTrap(i,7)+sedTrap(i,8)+sedTrap(i,9)+sedTrap(i,10))/day, &
               sedTrap(i,14)/day
               i = i + 1
            ENDDO
            j = 15
            DO WHILE (i .le. 26) 
               WRITE(53,200) sedTrap(i,1)/day, sedTrap(i,2)/day, sedTrap(i,3)/day,&
               sedTrap(i,4)/day, sedTrap(i,5)/day, sedTrap(i,6)/day, sedTrap(i,7)/day,&
               sedTrap(i,8)/day, sedTrap(i,9)/day, sedTrap(i,10)/day, sedTrap(i,11)/day,&
               sedTrap(i,12)/day, sedTrap(i,13)/day, time*timestep/86400, j*100, &
               (sedTrap(i,1)+sedTrap(i,2)+sedTrap(i,3)+sedTrap(i,4)+sedTrap(i,5)+&
               sedTrap(i,6)+sedTrap(i,7)+sedTrap(i,8)+sedTrap(i,9)+sedTrap(i,10))/day, &
               sedTrap(i,14)/day
               i = i + 1
               j = j + 5
            ENDDO
            sedTrap(:,:) = 0
         ENDIF

! update density, velocity, ... for all particles
      i = 1 !aggegate
      j = 1 !depth
      l = 1 !sizebin
      DO WHILE (i .lt. agg_max) 
         IF (agg(i)%b .eq. 0) THEN
            pa => agg(i)
            CALL find_depth(pa%z, j)
            CALL find_size(pa%r, l, n_size)
            CALL stickiness(pa,paraStick)
            CALL density(pa, paraFrac)
#ifdef microzoo
!            IF (pa%r .le. Fsize .and. mic(j)%Z .gt. 0) THEN
            IF (mic(j)%Z .gt. 0) THEN
               CALL microzoop(pa,mic(j),harvest,paraT,paraD,paraFrac,fudge)
            ENDIF
#endif
#ifdef respi
            CALL respiration(pa, 1, dummy, paraT)
#endif 
#ifdef dissol
            CALL dissolution(pa,paraC,paraT, paraFrac)
#endif
#ifdef disintegr
            CALL disintegrate(pa, harvest, paraFrac)
#endif
#ifdef agin
            CALL aging(pa)
#endif
#ifdef sinking
            CALL fractal_dimension(pa, paraFrac)
            CALL radius(pa, paraFrac)
            CALL velocity(pa, paraT)
            CALL sink(pa, time)
            CALL bottom(pa,l)
#endif
#ifdef dissolved
            CALL dissolve(pa)
#endif
#ifdef photo
            IF (pa%z .le. 100 .and. pa%r .gt. 1e4) THEN
               CALL photolysis(pa)
            ENDIF
#endif
#ifdef thorium
            CALL adsorb(pa, Th_d_234(j), Th_d_230(j))
            CALL radioactive_decay_particle(pa)
#endif
            CALL stickiness(pa,paraStick)
         ENDIF
         IF (agg(i)%b .eq. 1) THEN
            free_agg(free_count) = i
            free_count = free_count + 1
            pa => agg(i)
            CALL reset(pa)
         ENDIF
         i = i + 1
      ENDDO 
! tally thorium
      i = 1
      Th_total = 0
      DO WHILE (i .le. agg_max)
         Th_total = Th_total + agg(i)%Th_234*agg(i)%n
         i = i + 1
      ENDDO
!      print*, 'Thorium:', Th_total, time
      i = 1
      DO WHILE (i .le. seafloor/dz)
         Th_total = Th_total + Th_d_234(i)
         i = i + 1
      ENDDO
!      print*, 'Thorium total:', Th_total
! figure out how much stuff there is at each depth
      i = 1
      data_stuff = 0
      spec = 0
      age = 0
      DO WHILE (i .le. seafloor/dz) 
         data_stuff(1,i) = i*dz
         i = i + 1
      ENDDO 
      i = 1
      DO WHILE (i .le. 11) 
         age(1,i) = 2**(i-1)*5*86400
         i = i + 1
      ENDDO 
      i = 1
      DO WHILE (i .lt. agg_max) 
         pa => agg(i)
         IF (pa%z .lt. seafloor) THEN
            CALL collect(pa, data_stuff, spec, age)
         ENDIF
         i = i + 1
      ENDDO  
! write how much there is into a file
      i = 1
      IF(time .gt. endoftime - 90) then
         DO WHILE (i .le. seafloor/dz) 
            orgCtotal(i) = data_stuff(2,i)+data_stuff(3,i)+data_stuff(4,i)+&
                           data_stuff(5,i)+data_stuff(6,i)+data_stuff(7,i)+&
                           data_stuff(8,i)+data_stuff(9,i)+data_stuff(10,i)+&
                           data_stuff(11,i)
            calctotal(i) = data_stuff(12,i)+data_stuff(13,i)
            orgCMean(i) = orgCMean(i) + orgCtotal(i)
            calcMean(i) = calcMean(i) + calctotal(i)
            opalMean(i) = opalMean(i) + data_stuff(14,i) 
            clayMean(i) = clayMean(i) + data_stuff(15,i)
            i = i + 1
         ENDDO 
      ENDIF

! calculate all the properties to make pictures
#ifdef writing
      IF (time .eq. endoftime ) THEN
         data_mega(:,:,:,:) = 0
         i = 1
         DO WHILE (i .lt. agg_max) 
            pa => agg(i)
            IF (pa%z .lt. seafloor) THEN 
               CALL collect2(pa, data_mega)
               IF (pa%b .eq. 1) THEN 
                  print*, 'oppsie!', pa%id 
               ENDIF
            ENDIF
            i = i + 1
         ENDDO  
print*, 'ho ho'
         i = 1
         j = 1
         DO WHILE (j .le. seafloor/dz)
            DO WHILE (i .lt. n_size)  
               IF (data_mega(i,j,1,1) .gt. 0) THEN
                  data_mega(i,j,1,1) = data_mega(i,j,1,1)/data_mega(i,j,2,1)
               ENDIF
               WRITE(20,120) data_mega(i,j,1,1), radi(i), j !density

               IF (data_mega(i,j,1,2) .gt. 0) THEN
                  data_mega(i,j,1,2) = data_mega(i,j,1,2)/data_mega(i,j,2,2)
               ENDIF
               IF (data_mega(i,j,2,2) .le. 0) THEN
                  data_mega(i,j,1,2) = 0.0
               ENDIF
               WRITE(21,120) data_mega(i,j,1,2)*864, radi(i), j !velocity
               
               IF (data_mega(i,j,1,3) .gt. 0) THEN
                  data_mega(i,j,1,3) = data_mega(i,j,1,3)/data_mega(i,j,2,3)
               ENDIF
               IF (data_mega(i,j,2,3) .le. 0) THEN
                  data_mega(i,j,1,3) = 0
               ENDIF
               WRITE(22,120) data_mega(i,j,1,3), radi(i), j   !stickiness

               data_mega(i,j,1,4) = data_mega(i,j,1,4)!/(radius(i+1)-radius(i))
               WRITE(23,120) data_mega(i,j,1,4), radi(i), j       ! organic

               data_mega(i,j,1,5) = data_mega(i,j,1,5)!/(radius(i+1)-radius(i))
               WRITE(24,120) data_mega(i,j,1,5), radi(i), j       ! inorganic

               data_mega(i,j,1,6) = data_mega(i,j,1,6)!/(radius(i+1)-radius(i))
               WRITE(25,120) data_mega(i,j,1,6), radi(i), j       ! opal

               data_mega(i,j,1,7) = data_mega(i,j,1,7)!/(radius(i+1)-radius(i))
               WRITE(26,120) data_mega(i,j,1,7), radi(i), j       ! clay

               IF (data_mega(i,j,1,8) .gt. 0) THEN   ! rain ratio
                  data_mega(i,j,1,8) = data_mega(i,j,1,8)/data_mega(i,j,2,8)
               ENDIF
               WRITE(27,120) data_mega(i,j,1,8), radi(i), j

               IF (data_mega(i,j,1,9) .gt. 0) THEN   ! porosity
                  data_mega(i,j,1,9) = data_mega(i,j,1,9)/data_mega(i,j,2,9)
               ENDIF
               WRITE(46,120) data_mega(i,j,1,9), radi(i), j
               IF (data_mega(i,j,1,10) .gt. 0) THEN   ! thorium 234
                  data_mega(i,j,1,10) = data_mega(i,j,1,10)
               ENDIF
               WRITE(47,120) data_mega(i,j,1,10), radi(i), j

               IF (data_mega(i,j,1,11) .gt. 0) THEN   ! thorium 230
                  data_mega(i,j,1,11) = data_mega(i,j,1,11)
               ENDIF
               WRITE(54,120) data_mega(i,j,1,11), radi(i), j

               data_mega(i,j,1,12) = data_mega(i,j,1,12)!/(radius(i+1)-radius(i))
               WRITE(55,120) data_mega(i,j,1,12), radi(i), j       ! TEP

               i = i + 1
            ENDDO
            j = j + 1
            i = 1
         ENDDO
      ENDIF
#endif
      
! write the whole particle spectrum picture
#ifdef writing
      IF (MOD(time,1000) .eq. 0) THEN
         WRITE(ctime,*) time
         ctime = adjustl(ctime)
!         WRITE(file_name,"(a12)")("t"//trim(ctime)//".dat")
         WRITE(spec_name,"(a12)")("s"//trim(ctime)//".dat")
!         WRITE(age_name,"(a12)")("a"//trim(ctime)//".dat")
!         OPEN(UNIT=16, FILE=file_name, STATUS='NEW', POSITION='APPEND')
         OPEN(UNIT=17, FILE=spec_name, STATUS='NEW', POSITION='APPEND')
!         OPEN(UNIT=54, FILE=age_name, STATUS='NEW', POSITION='APPEND')
!         j = 1
!         i = 2
!         DO WHILE (j .le. seafloor/dz) 
!            DO WHILE (i .le. 11)  
!               WRITE(16,103) data_stuff(i,j), i, data_stuff(1,j)/100
!               i = i + 1
!            ENDDO
!            j = j + 1
!            i = 2
!         ENDDO
         j = 1 
         i = 1 
         DO WHILE (j .le. seafloor/dz)  
            DO WHILE (i .lt. n_size)   
               IF (i .eq. 1) THEN
                  WRITE(17,102) spec(1,j)/radi(1), radi(i), j*dz/100
               ELSEIF (i .gt. 1) THEN
                  WRITE(17,102) spec(i,j)/(radi(i+1)-radi(i)), radi(i),&
                                j*dz/100
               ENDIF
               i = i + 1
            ENDDO
            j = j + 1
            i = 1
         ENDDO
!         j = 1
!         i = 2
!         DO WHILE (j .le. seafloor/dz)  
!            DO WHILE (i .le. 10)   
!               WRITE(54,104) age(i,j), age(1,i)/86400, j*dz*1.0
!               i = i + 1
!            ENDDO
!            j = j + 1
!            i = 2
!         ENDDO
      ENDIF
#endif

#ifdef writing
      IF (time .eq. endoftime ) THEN
         i = 1
         DO WHILE (i .le. 10)  
            IF (i .eq. 1) THEN
               WRITE(28,122) seaBed(i), seaBed(i)/agg(i)%orgC(i,2)
            ELSEIF (i .gt. 1) THEN
               WRITE(28,122) seaBed(i), seaBed(i)/(agg(1)%orgC(i,2)-agg(1)%orgC(i-1,2))
            ENDIF
            i = i + 1
         ENDDO           
         DO WHILE (i .le. 19)  
            WRITE(29,122) seaBed(i)
            i = i + 1
         ENDDO
         WRITE(31,122) (seaBed(1)+seaBed(2)+seaBed(3)+seaBed(4)+seaBed(5)+&
                       seaBed(6)+seaBed(7)+seaBed(8)+seaBed(9)+seaBed(10))/&
                       (seaBed(11)+seaBed(12))
      ENDIF 

      i = 1
      DO WHILE (i .le. agg_max)
         IF (ABS(agg(i)%z - 100000) .le. dz/2.0) THEN
            pa => agg(i)
            CALL calculate_flux(pa, data_flux)
         ENDIF
         i = i + 1
      ENDDO
      WRITE(34,121) data_flux(1), time*timestep/86400.0
      WRITE(37,121) data_flux(2), time*timestep/86400.0
      WRITE(40,121) data_flux(3), time*timestep/86400.0
      WRITE(43,121) data_flux(1)/data_flux(2), time*timestep/86400.0
#endif

#ifdef writing
      IF (time .gt. 50 .and. MOD(time,100) .eq. 0) THEN
         WRITE(45, 145) sf(1)/(time*timestep/86400), &
                        sf(2)/(time*timestep/86400), &
                        sf(1)/sf(2), time*timestep/86400
      ENDIF
      i = 1
      organic = 0
      DO WHILE (i .le. 10) 
         organic = organic + agg(1)%orgC(i,1)
         i = i + 1
      ENDDO
      CALL find_depth(agg(1)%z, i)
      WRITE(48,147) time*timestep/86400.0, agg(1)%r, agg(1)%rho, agg(1)%w*864, &
      agg(1)%s, agg(1)%mineral(1)+agg(1)%mineral(2), agg(1)%mineral(3), &
      agg(1)%mineral(4),agg(1)%orgC(1,1)+agg(1)%orgC(2,1)+agg(1)%orgC(3,1), &
      agg(1)%orgC(4,1)+agg(1)%orgC(5,1)+agg(1)%orgC(6,1), & 
      agg(1)%orgC(7,1)+agg(1)%orgC(8,1)+agg(1)%orgC(9,1)+agg(1)%orgC(10,1), &
      agg(1)%z, agg(1)%Nn, agg(1)%pr, agg(1)%frac, agg(1)%n, agg(1)%por, &
      agg(1)%TEP(1,1)+agg(1)%TEP(2,1)+agg(1)%TEP(3,1)+agg(1)%TEP(4,1)+&
      agg(1)%TEP(5,1)+agg(1)%TEP(6,1)+agg(1)%TEP(7,1)+agg(1)%TEP(8,1)+&
      agg(1)%TEP(9,1)+agg(1)%TEP(10,1)
      
      CALL find_depth(agg(1)%z, i)
      WRITE(49,147) time*timestep/86400.0, agg(2)%r, agg(2)%rho, agg(2)%w*864, &
      agg(2)%s, agg(2)%mineral(1)+agg(2)%mineral(2), agg(2)%mineral(3), &
      agg(2)%mineral(4),agg(2)%orgC(1,1)+agg(2)%orgC(2,1)+agg(2)%orgC(3,1), &
      agg(2)%orgC(4,1)+agg(2)%orgC(5,1)+agg(2)%orgC(6,1), & 
      agg(2)%orgC(7,1)+agg(2)%orgC(8,1)+agg(2)%orgC(9,1)+agg(2)%orgC(10,1), &
      agg(2)%z, agg(2)%Nn, agg(2)%pr, agg(2)%frac, agg(2)%n, agg(2)%por, &
      agg(2)%TEP(1,1)+agg(2)%TEP(2,1)+agg(2)%TEP(3,1)+agg(2)%TEP(4,1)+&
      agg(2)%TEP(5,1)+agg(2)%TEP(6,1)+agg(2)%TEP(7,1)+agg(2)%TEP(8,1)+&
      agg(2)%TEP(9,1)+agg(2)%TEP(10,1)

      CALL find_depth(agg(1)%z, i)
      WRITE(50,147) time*timestep/86400.0, agg(3)%r, agg(3)%rho, agg(3)%w*864,&
      agg(3)%s, agg(3)%mineral(1)+agg(3)%mineral(2), agg(3)%mineral(3), &
      agg(3)%mineral(4), agg(3)%orgC(1,1)+agg(3)%orgC(2,1)+agg(3)%orgC(3,1),&
      agg(3)%orgC(4,1)+agg(3)%orgC(5,1)+agg(3)%orgC(6,1),&
      agg(3)%orgC(7,1)+agg(3)%orgC(8,1)+agg(3)%orgC(9,1)+agg(3)%orgC(10,1), &
      agg(3)%z, agg(3)%Nn, agg(3)%pr, agg(3)%frac, agg(3)%n, agg(3)%por, &
      agg(3)%TEP(1,1)+agg(3)%TEP(2,1)+agg(3)%TEP(3,1)+agg(3)%TEP(4,1)+&
      agg(3)%TEP(5,1)+agg(3)%TEP(6,1)+agg(3)%TEP(7,1)+agg(3)%TEP(8,1)+&
      agg(3)%TEP(9,1)+agg(3)%TEP(10,1)

      CALL find_depth(agg(1)%z, i)

      WRITE(51,147) time*timestep/86400.0, agg(11)%r, agg(11)%rho, agg(11)%w*864,&
      agg(11)%s, agg(11)%mineral(1)+agg(11)%mineral(2), agg(11)%mineral(3), &
      agg(11)%mineral(4), agg(11)%orgC(1,1)+agg(11)%orgC(2,1)+agg(11)%orgC(3,1), &
      agg(11)%orgC(4,1)+agg(11)%orgC(5,1)+agg(11)%orgC(6,1), &
      agg(11)%orgC(7,1)+agg(11)%orgC(8,1)+agg(11)%orgC(9,1)+agg(11)%orgC(10,1), &
      agg(11)%z, agg(11)%Nn, agg(11)%pr, agg(11)%frac, agg(11)%n, agg(11)%por,& 
      agg(11)%TEP(1,1)+agg(11)%TEP(2,1)+agg(11)%TEP(3,1)+agg(11)%TEP(4,1)+&
      agg(11)%TEP(5,1)+agg(11)%TEP(6,1)+agg(11)%TEP(7,1)+agg(11)%TEP(8,1)+&
      agg(11)%TEP(9,1)+agg(11)%TEP(10,1)
#endif

#ifdef writing
!write out the dissolved thorium with depth
      IF (MOD(time,400) .eq. 0) THEN
         i = 1
         Th_sum_234(:) = 0.0
         Th_sum_230(:) = 0.0
         mass_sum(:) = 0.0
         DO WHILE (i .le. agg_max)
            CALL find_depth(agg(i)%z, j)
            Th_sum_234(j) = Th_sum_234(j) + agg(i)%Th_234 * agg(i)%n
            Th_sum_230(j) = Th_sum_230(j) + agg(i)%Th_230 * agg(i)%n
            CALL particle_mass(agg(i),mass)
            mass_sum(j) = mass_sum(j) + mass
            i = i + 1
         ENDDO
         i = 1
         DO WHILE (i .le. seafloor/dz) 
            i = i + 1
         ENDDO
      ENDIF
#endif
      time = time + 1
      IF (time .eq. endoftime - 365*3) THEN
print*, 'time = ', time
         sizeBed(:) = 0
         veloBed(:) = 0
         densBed(:) = 0
         orgC_G1(:) = 0
         orgC_b(:) = 0
         calc_G1(:) = 0
         calcC_t(:) = 0
         calcA_t(:) = 0
         opal_t(:) = 0
         scaling(:,:) = 0
      ENDIF
   ENDDO

      
!write out the mean components of the component of the watercolumn.
   i = 1
   DO WHILE (i .le. seafloor/dz) !we divide by 90 because that is the number of ts 
      rainMean(i) = orgCMean(i)/calcMean(i) !that is averaged over to get orgCMean etc.
      WRITE(12,102) orgCMean(i)/90, data_stuff(1,i)
      WRITE(13,102) calcMean(i)/90, data_stuff(1,i)
      WRITE(14,102) opalMean(i)/90, data_stuff(1,i)
      WRITE(15,102) clayMean(i)/90, data_stuff(1,i)
      WRITE(150,102) rainMean(i), data_stuff(1,i)
      i = i + 1
   ENDDO
   i = 1
   DO WHILE (i .lt. agg_max) 
      j = 1
      DO WHILE (j .le. 10) 
         mass_f(1) = mass_f(1) + agg(i)%orgC(j,1)*agg(i)%n
         mass_f(5) = mass_f(5) + agg(i)%TEP(j,1)*agg(i)%n
         j = j + 1
      ENDDO
      mass_f(2) = mass_f(2) + (agg(i)%mineral(1)+agg(i)%mineral(2))*agg(i)%n
      mass_f(3) = mass_f(3) + agg(i)%mineral(3)*agg(i)%n
      mass_f(4) = mass_f(4) + agg(i)%mineral(4)*agg(i)%n
      CALL find_size(agg(i)%r, l, n_size)
      scaling(1,l) = scaling(1,l) + 1
      scaling(2,l) = scaling(2,l) + agg(i)%n
      i = i + 1
   ENDDO

   print*, npp(1), sf(1), 'orgC prod - sf'
   print*, npp(2), sf(2), 'calc prod - sf'
   print*, npp(3), sf(3), 'opal prod - sf'
   print*, npp(4), sf(4), 'clay prod - sf'
   print*, npp(5), sf(5), 'TEP prod - sf'

   print*, mass_i(1), lost(1), mass_f(1) + lost(1) + sf(1) 
   print*, mass_i(2), lost(2), mass_f(2) + lost(2) + sf(2) 
   print*, mass_i(3), lost(3), mass_f(3) + lost(3) + sf(3) 
   print*, mass_i(4), lost(4), mass_f(4) + lost(4) + sf(4) 
   print*, mass_i(5), lost(5), mass_f(5) + lost(5) + sf(5) 

   WRITE(18,118) sf(1), sf(2), sf(3), sf(4), sf(5)
   WRITE(19,118) npp(1), npp(2), npp(3), npp(4), npp(5)
   i = 1
   DO WHILE (i .le. n_size) 
      IF (i .eq. 1) THEN
         WRITE(104,104) sizeBed(i), radi(i), sizeBed(i)/radi(i)
         WRITE(111,104) veloBed(i), radi(i), veloBed(i)/(radi(i)/864)
         WRITE(112,104) densBed(i), 0.8+0.03*(i-1), densBed(i)/0.03
         WRITE(113,245) scaling(1,i), scaling(2,i)/radi(i), radi(i)
      ELSE
         WRITE(104,104) sizeBed(i), radi(i), sizeBed(i)/(radi(i)-radi(i-1))
         WRITE(111,104) veloBed(i), radi(i), veloBed(i)/((radi(i)-radi(i-1))/864)
         WRITE(112,104) densBed(i), 0.8+0.03*(i-1), densBed(i)/0.03
         WRITE(113,245) scaling(1,i), scaling(2,i)/(radi(i)-radi(i-1)), radi(i)
      ENDIF
      i = i + 1
   ENDDO

   WRITE(parT,*) intT
   parT = adjustl(parT)
   WRITE(parC,*) intC
   parC = adjustl(parC)
   WRITE(parP,*) intP
   parP = adjustl(parP)
   WRITE(parS,*) intTEP
   parS = adjustl(parS)
   WRITE(file_name,"(a30)")("consu")!//trim(parT)//trim(parC)//trim(parP)
   OPEN(UNIT=210, FILE=file_name, STATUS='NEW', POSITION='APPEND')
   i = 1
   DO WHILE (i .le. seafloor/dz) 
      orgC_G1(i) = orgC_G1(i)/(dz/100)
      orgC_b(i)  = orgC_b(i)/(dz/100)
      calc_G1(i) = calc_G1(i)/(dz/100)
      calcC_t(i) = calcC_t(i)/(dz/100)
      calcA_t(i) = calcA_t(i)/(dz/100)
      opal_t(i)  = opal_t(i)/(dz/100)
      WRITE(210,260) i*dz/100, orgC_G1(i), orgC_b(i), &
      calc_G1(i), calcC_t(i), calcA_t(i), opal_t(i)
      i = i + 1
   ENDDO

END PROGRAM
