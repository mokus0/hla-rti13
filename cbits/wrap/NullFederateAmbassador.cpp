#include "wrap/NullFederateAmbassador.h"

#include "wrap/rti.h"

ccall void *wrap_new_NullFederateAmbassador(void **out_exc) {
    wrap(return new NullFedAmb())
}

ccall void *wrap_delete_NullFederateAmbassador(void *fedAmb, void **out_exc) {
    wrap(delete (NullFedAmb *)fedAmb)
}

