#include "wrap/BaseTypes.h"
#include "wrap/rti.h"

ccall void delete_Exception(void *exc) {
    delete (rti13::Exception *) exc;
}

ccall void dissect_Exception(void *exc, RTI_ULong *serial, const char **reason, const char **name) {
//    try {
        rti13::Exception *e = (rti13::Exception *) exc;
        
        *serial = e->_serial;
        *reason = e->_reason;
        *name   = e->_name;
//    } catch (...) {
//        RTI_STD::cout << "FATAL: got unknown exception in dissect_Exception" << RTI_STD::endl;
//        throw;
//    }
}

