!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
!
!    This file is part of ExtLoads.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!**********************************************************************************************************************************
! File last committed: $Date$
! (File) Revision #: $Rev$
! URL: $HeadURL$
!**********************************************************************************************************************************
!> ExtPtfmLoads is a time-domain loads module for horizontal-axis wind turbines.
module ExtPtfmLoads

   use NWTC_Library
   use ExtPtfmLoads_Types
   use InflowWind_IO_Types
   use InflowWind_IO

   implicit none

   private

   ! ..... Public Subroutines ...................................................................................................

   public :: ExtPtfmLd_Init                           ! Initialization routine
   public :: ExtPtfmLd_End                            ! Ending routine (includes clean up)
   public :: ExtPtfmLd_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                     !   continuous states, and updating discrete states
   public :: ExtPtfmLd_CalcOutput                     ! Routine for computing outputs
   public :: ExtPtfmLd_ConvertOpDataForOpenFAST        ! Routine to convert Output data for OpenFAST
   public :: ExtPtfmLd_ConvertInpDataForExtProg        ! Routine to convert Input data for external programs

contains    

!> Helper functions for the module

!> This routine sets the error status and error message for a routine, it's a simplified version of SetErrStat from NWTC_Library
subroutine SetErrStatSimple(ErrStat, ErrMess, RoutineName, LineNumber)
  INTEGER(IntKi), INTENT(INOUT)        :: ErrStat      ! Error status of the operation
  CHARACTER(*),   INTENT(INOUT)        :: ErrMess      ! Error message if ErrStat /= ErrID_None
  CHARACTER(*),   INTENT(IN   )        :: RoutineName  ! Name of the routine error occurred in
  INTEGER(IntKi), INTENT(IN), OPTIONAL :: LineNumber   ! Line of input file 
  if (ErrStat /= ErrID_None) then
     write(ErrMess,'(A)') TRIM(RoutineName)//':'//TRIM(ErrMess)
     if (present(LineNumber)) then
         ErrMess = TRIM(ErrMess)//' Line: '//TRIM(Num2LStr(LineNumber))//'.'
     endif
  end if
end subroutine SetErrStatSimple
!----------------------------------------------------------------------------------------------------------------------------------   
!> This subroutine sets the initialization output data structure, which contains data to be returned to the calling program (e.g.,
!! FAST)   
subroutine ExtPtfmLd_SetInitOut(p, InitOut, errStat, errMsg)

   type(ExtPtfmLd_InitOutputType),    intent(inout)  :: InitOut          ! output data
   type(ExtPtfmLd_ParameterType),     intent(in   )  :: p                ! Parameters
   integer(IntKi),                intent(  out)  :: errStat          ! Error status of the operation
   character(*),                  intent(  out)  :: errMsg           ! Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_SetInitOut'
   
   
   
   integer(IntKi)                               :: i, j, k, f
   integer(IntKi)                               :: NumCoords
#ifdef DBG_OUTS
   integer(IntKi)                               :: m
   character(5)                                 ::chanPrefix
#endif   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   
end subroutine ExtPtfmLd_SetInitOut

!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine is called at the start of the simulation to perform initialization steps.
!! The parameters are set here and not changed during the simulation.
!! The initial states and initial guess for the input are defined.
SUBROUTINE ExtPtfmLd_Init( InitInp, u, p, x, xd, z, OtherState, y, m, Interval, InitOut, ErrStat, ErrMsg )
!..................................................................................................................................

   TYPE(ExtPtfmLd_InitInputType),       INTENT(IN   )  :: InitInp     !< Input data for initialization routine
   TYPE(ExtPtfmLd_InputType),           INTENT(  OUT)  :: u           !< An initial guess for the input; input mesh must be defined
   TYPE(ExtPtfmLd_ParameterType),       INTENT(  OUT)  :: p           !< Parameters
   TYPE(ExtPtfmLd_ContinuousStateType), INTENT(  OUT)  :: x           !< Initial continuous states
   TYPE(ExtPtfmLd_DiscreteStateType),   INTENT(  OUT)  :: xd          !< Initial discrete states
   TYPE(ExtPtfmLd_ConstraintStateType), INTENT(  OUT)  :: z           !< Initial guess of the constraint states
   TYPE(ExtPtfmLd_OtherStateType),      INTENT(  OUT)  :: OtherState  !< Initial other states (logical, etc)
   TYPE(ExtPtfmLd_OutputType),          INTENT(  OUT)  :: y           !< Initial system outputs (outputs are not calculated;
                                                                     !!   only the output mesh is initialized)
   TYPE(ExtPtfmLd_MiscVarType),         INTENT(  OUT)  :: m           !< Misc variables for optimization (not copied in glue code)
   REAL(DbKi),                        INTENT(INOUT)  :: Interval    !< Coupling interval in seconds: the rate that
                                                                     !!   (1) ExtPtfmLd_UpdateStates() is called in loose coupling &
                                                                     !!   (2) ExtPtfmLd_UpdateDiscState() is called in tight coupling.
                                                                     !!   Input is the suggested time from the glue code;
                                                                     !!   Output is the actual coupling interval that will be used
                                                                     !!   by the glue code.
   TYPE(ExtPtfmLd_InitOutputType),      INTENT(  OUT)  :: InitOut     !< Output for initialization routine
   INTEGER(IntKi),                    INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                      INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None

      ! Local variables
   integer(IntKi)                              :: i             ! loop counter
   type(Points_InitInputType)                  :: Points_InitInput
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
      
   character(*), parameter                     :: RoutineName = 'ExtPtfmLd_Init'
   
   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""

   call NWTC_Init( )

      ! Initialize the NWTC Subroutine Library

   call Init_meshes(u, y, InitInp, ErrStat2, ErrMsg2); if(Failed()) return

   !............................................................................................
   ! Define and initialize inputs here 
   !............................................................................................

   CALL AllocPAry( u%DX_u%ptfmDef, 3+9+3+3+3+3, 'ptfmDef', ErrStat2, ErrMsg2 ); if(Failed()) return

   u%DX_u%C_obj%ptfmDef_Len = 3+9+3+3+3+3
   u%DX_u%C_obj%ptfmDef = C_LOC(u%DX_u%ptfmDef(1))


      
   !............................................................................................
   ! Define initialization output here
   !............................................................................................

   CALL AllocPAry( y%DX_y%ptfmLd, 6, 'ptfmLd', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   y%DX_y%c_obj%ptfmLd_Len = 6; y%DX_y%c_obj%ptfmLd = C_LOC( y%DX_y%ptfmLd(1) )

   call ExtPtfmLd_SetInitOut(p, InitOut, errStat2, errMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
   

contains
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
   end function Failed
 
end subroutine ExtPtfmLd_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_meshes(u, y, InitInp, ErrStat, ErrMsg)
   TYPE(ExtPtfmLd_InputType),           INTENT(INOUT)  :: u           !< System inputs
   TYPE(ExtPtfmLd_OutputType),          INTENT(INOUT)  :: y           !< System outputs
   TYPE(ExtPtfmLd_InitInputType),       INTENT(IN   )  :: InitInp     !< Input data for initialization routine
   INTEGER(IntKi),                    INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                      INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None
   ! Create the input and output meshes associated with platform loads
   CALL MeshCreate(  BlankMesh         = u%PtfmMotion     , &
                     IOS               = COMPONENT_INPUT  , &
                     Nnodes            = 1                , &
                     ErrStat           = ErrStat          , &
                     ErrMess           = ErrMsg           , &
                     TranslationDisp   = .TRUE.           , &
                     Orientation       = .TRUE.           , &
                     TranslationVel    = .TRUE.           , &
                     RotationVel       = .TRUE.           , &
                     TranslationAcc    = .TRUE.           , &
                     RotationAcc       = .TRUE.)
   if(Failed()) return
      
   ! Create the node on the mesh, the node is located at the PlatformRefzt, to match ElastoDyn
   CALL MeshPositionNode (u%PtfmMotion, 1, (/0.0_ReKi, 0.0_ReKi, InitInp%PtfmRefzt/), ErrStat, ErrMsg ); if(Failed()) return
   ! Create the mesh element
   CALL MeshConstructElement (  u%PtfmMotion, ELEMENT_POINT, ErrStat, ErrMsg, 1 ); if(Failed()) return
   CALL MeshCommit ( u%PtfmMotion, ErrStat, ErrMsg ); if(Failed()) return
   ! the output mesh is a sibling of the input:
   CALL MeshCopy( SrcMesh=u%PtfmMotion, DestMesh=y%PtfmMesh, CtrlCode=MESH_SIBLING, IOS=COMPONENT_OUTPUT, &
                  ErrStat=ErrStat, ErrMess=ErrMsg, Force=.TRUE., Moment=.TRUE. )
   if(Failed()) return
CONTAINS
    logical function Failed()
        CALL SetErrStatSimple(ErrStat, ErrMsg, 'Init_meshes')
        Failed =  ErrStat >= AbortErrLev
    end function Failed
END SUBROUTINE Init_meshes
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine converts the displacement data in the meshes in the input into a simple array format that can be accessed by external programs
subroutine ExtPtfmLd_ConvertInpDataForExtProg(u, p, errStat, errMsg )
!..................................................................................................................................
  USE BeamDyn_IO, ONLY: BD_CrvExtractCrv
  
   type(ExtPtfmLd_InputType),           intent(inout)  :: u                 !< Input data
   type(ExtPtfmLd_ParameterType),       intent(in   )  :: p                 !< Parameters
   integer(IntKi),               intent(  out)  :: errStat           !< Error status of the operation
   character(*),                 intent(  out)  :: errMsg            !< Error message if ErrStat /= ErrID_None


      ! Local variables
   real(R8Ki)                                   :: wm_crv(3)         ! Wiener-Milenkovic parameters
   integer(intKi)                               :: j                 ! counter for nodes
   integer(intKi)                               :: jTot              ! counter for nodes
   integer(intKi)                               :: k                 ! counter for blades
   real(reki)                                   :: cref(3)
   real(reki)                                   :: xloc(3)
   real(reki)                                   :: yloc(3)
   real(reki)                                   :: zloc(3)
   
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_ConvertInpDataForExtProg'

      ! Initialize variables for this routine

   ErrStat = ErrID_None
   ErrMsg  = ""

   u%DX_u%ptfmDef(1:3) = u%PtfmMotion%TranslationDisp(:, 1)
   u%DX_u%ptfmDef(4:6) = u%PtfmMotion%Orientation(:, 1, 1)
   u%DX_u%ptfmDef(7:9) = u%PtfmMotion%Orientation(:, 2, 1)
   u%DX_u%ptfmDef(10:12) = u%PtfmMotion%Orientation(:, 3, 1)
   u%DX_u%ptfmDef(13:15) = u%PtfmMotion%TranslationVel(:, 1)
   u%DX_u%ptfmDef(16:18) = u%PtfmMotion%RotationVel(:, 1)
   u%DX_u%ptfmDef(19:21) = u%PtfmMotion%TranslationAcc(:, 1)
   u%DX_u%ptfmDef(22:24) = u%PtfmMotion%RotationAcc(:, 1)

   ! print *, "---- Platform State ----"
   ! write(*,'(A,3F10.5)') "Displacement (m): ", u%PtfmMotion%TranslationDisp(:, 1)

   ! print *, "Orientation matrix:"
   ! write(*,'(3F10.5)') u%PtfmMotion%Orientation(:, 1, 1)
   ! write(*,'(3F10.5)') u%PtfmMotion%Orientation(:, 2, 1)
   ! write(*,'(3F10.5)') u%PtfmMotion%Orientation(:, 3, 1)

   ! write(*,'(A,3F10.5)') "Trans Vel (m/s): ", u%PtfmMotion%TranslationVel(:, 1)
   ! write(*,'(A,3F10.5)') "Rot Vel (rad/s): ", u%PtfmMotion%RotationVel(:, 1)
   ! print *, "------------------------"
   ! print *,"C_LOC(u%DX_u%ptfmDef(1)) = ", C_LOC(u%DX_u%ptfmDef(1))
   ! write(*, '(A, 3F10.5)') "u.DX_u.ptfmDef: ", u%DX_u%ptfmDef(1:3)
   
end subroutine ExtPtfmLd_ConvertInpDataForExtProg
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine converts the data in the simple array format in the output data type into OpenFAST mesh format
subroutine ExtPtfmLd_ConvertOpDataForOpenFAST(y, u, m, p, errStat, errMsg )
!..................................................................................................................................
  
   type(ExtPtfmLd_OutputType),          intent(inout)  :: y                 !< Ouput data
   type(ExtPtfmLd_InputType),           intent(in   )  :: u                 !< Input data
   type(ExtPtfmLd_MiscVarType),         intent(inout)  :: m                 !< Misc var
   type(ExtPtfmLd_ParameterType),       intent(in   )  :: p                 !< Parameters
   integer(IntKi),               intent(  out)  :: errStat           !< Error status of the operation
   character(*),                 intent(  out)  :: errMsg            !< Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: j                 ! counter for nodes
   integer(intKi)                               :: jTot              ! counter for nodes
   integer(intKi)                               :: k                 ! counter for blades
   real(ReKi)                                   :: tmp_az, delta_az  ! temporary variable for azimuth
   
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_ConvertInpDataForExtProg'

      ! Initialize variables for this routine

   ErrStat = ErrID_None
   ErrMsg  = ""

   y%PtfmMesh%Force(:, 1) = y%DX_y%ptfmLd(1:3)
   y%PtfmMesh%Moment(:, 1) = y%DX_y%ptfmLd(4:6)

end subroutine ExtPtfmLd_ConvertOpDataForOpenFAST
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine is called at the end of the simulation.
subroutine ExtPtfmLd_End( u, p, x, xd, z, OtherState, y, m, ErrStat, ErrMsg )
!..................................................................................................................................

      TYPE(ExtPtfmLd_InputType),           INTENT(INOUT)  :: u           !< System inputs
      TYPE(ExtPtfmLd_ParameterType),       INTENT(INOUT)  :: p           !< Parameters
      TYPE(ExtPtfmLd_ContinuousStateType), INTENT(INOUT)  :: x           !< Continuous states
      TYPE(ExtPtfmLd_DiscreteStateType),   INTENT(INOUT)  :: xd          !< Discrete states
      TYPE(ExtPtfmLd_ConstraintStateType), INTENT(INOUT)  :: z           !< Constraint states
      TYPE(ExtPtfmLd_OtherStateType),      INTENT(INOUT)  :: OtherState  !< Other states
      TYPE(ExtPtfmLd_OutputType),          INTENT(INOUT)  :: y           !< System outputs
      TYPE(ExtPtfmLd_MiscVarType),         INTENT(INOUT)  :: m           !< Misc/optimization variables
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:


         ! Destroy the input data:


END SUBROUTINE ExtPtfmLd_End
!----------------------------------------------------------------------------------------------------------------------------------
!> Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete and other states.
! Continuous, constraint, discrete, and other states are updated for t + Interval
subroutine ExtPtfmLd_UpdateStates( t, n, u, utimes, p, x, xd, z, OtherState, m, errStat, errMsg )
!..................................................................................................................................

   real(DbKi),                     intent(in   ) :: t          !< Current simulation time in seconds
   integer(IntKi),                 intent(in   ) :: n          !< Current simulation time step n = 0,1,...
   type(ExtPtfmLd_InputType),             intent(inout) :: u(:)       !< Inputs at utimes (out only for mesh record-keeping in ExtrapInterp routine)
   real(DbKi),                     intent(in   ) :: utimes(:)  !< Times associated with u(:), in seconds
   type(ExtPtfmLd_ParameterType),         intent(in   ) :: p          !< Parameters
   type(ExtPtfmLd_ContinuousStateType),   intent(inout) :: x          !< Input: Continuous states at t;
                                                               !!   Output: Continuous states at t + Interval
   type(ExtPtfmLd_DiscreteStateType),     intent(inout) :: xd         !< Input: Discrete states at t;
                                                               !!   Output: Discrete states at t  + Interval
   type(ExtPtfmLd_ConstraintStateType),   intent(inout) :: z          !< Input: Constraint states at t;
                                                               !!   Output: Constraint states at t+dt
   type(ExtPtfmLd_OtherStateType),        intent(inout) :: OtherState !< Input: Other states at t;
                                                               !!   Output: Other states at t+dt
   type(ExtPtfmLd_MiscVarType),           intent(inout) :: m          !< Misc/optimization variables
   integer(IntKi),                 intent(  out) :: errStat    !< Error status of the operation
   character(*),                   intent(  out) :: errMsg     !< Error message if ErrStat /= ErrID_None

   ! local variables
   type(ExtPtfmLd_InputType)                           :: uInterp     ! Interpolated/Extrapolated input
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_UpdateStates'
      
   ErrStat = ErrID_None
   ErrMsg  = ""
           
   
end subroutine ExtPtfmLd_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine for computing outputs, used in both loose and tight coupling.
!! This subroutine is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
!! The descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
!! for a complete description of each output parameter.
subroutine ExtPtfmLd_CalcOutput( t, u, p, x, xd, z, OtherState, y, m, ErrStat, ErrMsg )
! NOTE: no matter how many channels are selected for output, all of the outputs are calculated
! All of the calculated output channels are placed into the m%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................

   REAL(DbKi),                   INTENT(IN   )  :: t           !< Current simulation time in seconds
   TYPE(ExtPtfmLd_InputType),           INTENT(IN   )  :: u           !< Inputs at Time t
   TYPE(ExtPtfmLd_ParameterType),       INTENT(IN   )  :: p           !< Parameters
   TYPE(ExtPtfmLd_ContinuousStateType), INTENT(IN   )  :: x           !< Continuous states at t
   TYPE(ExtPtfmLd_DiscreteStateType),   INTENT(IN   )  :: xd          !< Discrete states at t
   TYPE(ExtPtfmLd_ConstraintStateType), INTENT(IN   )  :: z           !< Constraint states at t
   TYPE(ExtPtfmLd_OtherStateType),      INTENT(IN   )  :: OtherState  !< Other states at t
   TYPE(ExtPtfmLd_OutputType),          INTENT(INOUT)  :: y           !< Outputs computed at t (Input only so that mesh con-
                                                               !!   nectivity information does not have to be recalculated)
   type(ExtPtfmLd_MiscVarType),         intent(inout)  :: m           !< Misc/optimization variables
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None


   integer, parameter                           :: indx = 1  ! m%BEMT_u(1) is at t; m%BEMT_u(2) is t+dt
   integer(intKi)                               :: i
   integer(intKi)                               :: j

   integer(intKi)                               :: ErrStat2
   character(ErrMsgLen)                         :: ErrMsg2
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_CalcOutput'

   ErrStat = ErrID_None
   ErrMsg  = ""

 end subroutine ExtPtfmLd_CalcOutput

END MODULE ExtPtfmLoads
