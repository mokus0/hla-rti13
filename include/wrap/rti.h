#ifndef ___n_rti_h__
#define ___n_rti_h__

#ifdef RTI_NG_API
#   ifdef DLC_API
#       error RTI_NG_API and DLC_API are both set!  That's just silly!
#   endif
#   include "RTI.hh"
#   include "NullFederateAmbassador.hh"
#   include "fedtime.hh"
#   define rti13 RTI
#   define NullFedAmb NullFederateAmbassador
#elif defined(DLC_API)
#   include "RTI13.h"
#   include "NullFederateAmbassador13.h"
#   include "fedtime13.h"
#   define NullFedAmb rti13::NullFederateAmbassador
#else
#   error RTI interface version macro not set (#define either RTI_NG_API or DLC_API)
#endif

#endif /* ___n_rti_h__ */

