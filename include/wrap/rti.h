#ifndef ___n_rti_h__
#define ___n_rti_h__

#ifdef RTI_NG_MATREX_API
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
#endif

#endif /* ___n_rti_h__ */

