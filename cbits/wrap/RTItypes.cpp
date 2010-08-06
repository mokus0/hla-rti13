#include "wrap/RTItypes.h"

#include "wrap/rti.h"

ccall const char *wrap_DEFAULT_SPACE_NAME()               { return DEFAULT_SPACE_NAME;           }
ccall const char *wrap_DEFAULT_SPACE_DIMENSION_NAME()     { return DEFAULT_SPACE_DIMENSION_NAME; }

ccall const char *wrap_RTI_VERSION()                  { return RTI_VERSION;          }
ccall const char *wrap_RTI_INTERNAL_VERSION()         { return RTI_INTERNAL_VERSION; }

ccall RTI_ULong wrap_RTI_MAJOR_VERSION()            { return RTI_MAJOR_VERSION; }
ccall RTI_ULong wrap_RTI_MINOR_VERSION()            { return RTI_MINOR_VERSION; }
ccall RTI_ULong wrap_RTI_RELEASE()                  { return RTI_RELEASE;       }

ccall RTI_ULong wrap_RTI_INTERNAL_MAJOR_VERSION()   { return RTI_INTERNAL_MAJOR_VERSION; }
ccall RTI_ULong wrap_RTI_INTERNAL_MINOR_VERSION()   { return RTI_INTERNAL_MINOR_VERSION; }
ccall RTI_ULong wrap_RTI_INTERNAL_RELEASE()         { return RTI_INTERNAL_RELEASE;       }

ccall RTI_ULong wrap_MIN_EXTENT()                   { return MIN_EXTENT; }
ccall RTI_ULong wrap_MAX_EXTENT()                   { return MAX_EXTENT; }

///// AttributeHandleValuePairSet
#define invoke(method)  wrap(return ((rti13::AttributeHandleValuePairSet *)ahvpSet)->method)

ccall void wrap_delete_AttributeHandleValuePairSet(void *ahvpSet, void **out_exc) {
    wrap(delete ((rti13::AttributeHandleValuePairSet *)ahvpSet))
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_size(void *ahvpSet, void **out_exc) {
    invoke(size())
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_getHandle(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(getHandle(i))
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_getValueLength(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(getValueLength(i))
}

ccall void      wrap_AttributeHandleValuePairSet_getValue(void *ahvpSet, RTI_ULong i, char *buff, RTI_ULong *valueLength, void **out_exc) {
    invoke(getValue(i,buff,*valueLength))
}

ccall char *wrap_AttributeHandleValuePairSet_getValuePointer(void *ahvpSet, RTI_ULong i, RTI_ULong *valueLength, void **out_exc) {
    invoke(getValuePointer(i, *valueLength))
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_getTransportType(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(getTransportType(i))
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_getOrderType(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(getOrderType(i))
}

ccall void     *wrap_AttributeHandleValuePairSet_getRegion(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(getRegion(i))
}

ccall void wrap_AttributeHandleValuePairSet_add(void *ahvpSet, RTI_ULong ah, const char *buf, RTI_ULong len, void **out_exc) {
    invoke(add(ah, buf, len))
}

ccall void wrap_AttributeHandleValuePairSet_remove(void *ahvpSet, RTI_ULong ah, void **out_exc) {
    invoke(remove(ah))
}

ccall void wrap_AttributeHandleValuePairSet_moveFrom(void *ahvpSet, void *from, RTI_ULong *i, void **out_exc) {
    invoke(moveFrom(*(rti13::AttributeHandleValuePairSet *)from, *i))
}

ccall void wrap_AttributeHandleValuePairSet_empty(void *ahvpSet, void **out_exc) {
    invoke(empty())
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_start(void *ahvpSet, void **out_exc) {
    invoke(start())
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_valid(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(valid(i))
}

ccall RTI_ULong wrap_AttributeHandleValuePairSet_next(void *ahvpSet, RTI_ULong i, void **out_exc) {
    invoke(next(i))
}

#undef invoke

///// AttributeSetFactory
ccall void *wrap_AttributeSetFactory_create(RTI_ULong n, void **out_exc) {
    wrap(return rti13::AttributeSetFactory::create(n))
}

///// AttributeHandleSet
#define invoke(method)  wrap(return ((rti13::AttributeHandleSet *)ahSet)->method)

ccall void wrap_delete_AttributeHandleSet(void *ahSet, void **out_exc) {
    wrap(delete ((rti13::AttributeHandleSet *)ahSet))
}

ccall RTI_ULong wrap_AttributeHandleSet_size(void *ahSet, void **out_exc) {
    invoke(size())
}

ccall RTI_ULong wrap_AttributeHandleSet_getHandle(void *ahSet, RTI_ULong i, void **out_exc) {
    invoke(getHandle(i))
}

ccall void wrap_AttributeHandleSet_add(void *ahSet, RTI_ULong ah, void **out_exc) {
    invoke(add(ah))
}

ccall void wrap_AttributeHandleSet_remove(void *ahSet, RTI_ULong ah, void **out_exc) {
    invoke(remove(ah))
}

ccall void wrap_AttributeHandleSet_empty(void *ahSet, void **out_exc) {
    invoke(empty())
}

ccall HsBool wrap_AttributeHandleSet_isEmpty(void *ahSet, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::AttributeHandleSet *)ahSet)->isEmpty();
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}

ccall HsBool wrap_AttributeHandleSet_isMember(void *ahSet, RTI_ULong h, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::AttributeHandleSet *)ahSet)->isMember(h);
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}

#undef invoke

///// AttributeHandleSetFactory
ccall void *wrap_AttributeHandleSetFactory_create(RTI_ULong n, void **out_exc) {
    wrap(return rti13::AttributeHandleSetFactory::create(n))
}

///// FederateHandleSet
#define invoke(method)  wrap(return ((rti13::FederateHandleSet *)fhSet)->method)

ccall void      wrap_delete_FederateHandleSet(void *fhSet, void **out_exc) {
    wrap(
        delete ((rti13::FederateHandleSet *)fhSet);
    )
}

ccall RTI_ULong wrap_FederateHandleSet_size(void *fhSet, void **out_exc) {
    invoke(size())
}

ccall RTI_ULong wrap_FederateHandleSet_getHandle(void *fhSet, RTI_ULong i, void **out_exc) {
    invoke(getHandle(i))
}

ccall void      wrap_FederateHandleSet_add(void *fhSet, RTI_ULong ah, void **out_exc) {
    invoke(add(ah))
}

ccall void      wrap_FederateHandleSet_remove(void *fhSet, RTI_ULong ah, void **out_exc) {
    invoke(remove(ah))
}

ccall void      wrap_FederateHandleSet_empty(void *fhSet, void **out_exc) {
    invoke(empty())
}

ccall HsBool    wrap_FederateHandleSet_isMember(void *fhSet, RTI_ULong h, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::FederateHandleSet *)fhSet)->isMember(h);
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}

#undef invoke

///// FederateHandleSetFactory
ccall void     *wrap_FederateHandleSetFactory_create(RTI_ULong n, void **out_exc) {
    wrap (return rti13::FederateHandleSetFactory::create(n))
}

///// ParameterHandleValuePairSet
#define invoke(method)  wrap(return ((rti13::ParameterHandleValuePairSet *)phvpSet)->method)

ccall void wrap_delete_ParameterHandleValuePairSet(void *pSet, void **out_exc) {
    wrap(
        delete ((rti13::ParameterHandleValuePairSet *)pSet);
    )
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_size(void *phvpSet, void **out_exc) {
    invoke(size())
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_getHandle(void *phvpSet, RTI_ULong i, void **out_exc) {
    invoke(getHandle(i))
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_getValueLength(void *phvpSet, RTI_ULong i, void **out_exc) {
    invoke(getValueLength(i))
}

ccall void      wrap_ParameterHandleValuePairSet_getValue(void *phvpSet, RTI_ULong i, char *buff, RTI_ULong *valueLength, void **out_exc) {
    invoke(getValue(i, buff, *valueLength))
}

ccall char *wrap_ParameterHandleValuePairSet_getValuePointer(void *phvpSet, RTI_ULong i, RTI_ULong *valueLength, void **out_exc) {
    invoke(getValuePointer(i, *valueLength))
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_getTransportType(void *phvpSet, void **out_exc) {
    invoke(getTransportType())
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_getOrderType(void *phvpSet, void **out_exc) {
    invoke(getOrderType())
}

ccall void     *wrap_ParameterHandleValuePairSet_getRegion(void *phvpSet, void **out_exc) {
    invoke(getRegion())
}

ccall void wrap_ParameterHandleValuePairSet_add(void *phvpSet, RTI_ULong h, const char *buf, RTI_ULong len, void **out_exc) {
    invoke(add(h, buf, len))
}

ccall void wrap_ParameterHandleValuePairSet_remove(void *phvpSet, RTI_ULong h, void **out_exc) {
    invoke(remove(h))
}

ccall void      wrap_ParameterHandleValuePairSet_moveFrom(void *phvpSet, const void *from, RTI_ULong *i, void **out_exc) {
    invoke(moveFrom(*(const rti13::ParameterHandleValuePairSet *)from, *i))
}

ccall void      wrap_ParameterHandleValuePairSet_empty(void *phvpSet, void **out_exc) {
    invoke(empty())
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_start(void *phvpSet, void **out_exc) {
    invoke(start())
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_valid(void *phvpSet, RTI_ULong i, void **out_exc) {
    invoke(valid(i))
}

ccall RTI_ULong wrap_ParameterHandleValuePairSet_next(void *phvpSet, RTI_ULong i, void **out_exc) {
    invoke(next(i))
}

#undef invoke

///// ParameterSetFactory
ccall void *wrap_ParameterSetFactory_create(RTI_ULong count, void **out_exc) {
    wrap(
        return rti13::ParameterSetFactory::create(count);
    )
}

///// Region
#define invoke(method)  wrap(return ((rti13::Region *)theRegion)->method)

ccall void wrap_delete_Region(void *theRegion, void **out_exc) {
    wrap(delete ((rti13::Region *)theRegion))
}

ccall RTI_ULong wrap_Region_getRangeLowerBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc) {
    invoke(getRangeLowerBound(theExtent, theDimension))
}

ccall RTI_ULong wrap_Region_getRangeUpperBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc) {
    invoke(getRangeUpperBound(theExtent, theDimension))
}

ccall void wrap_Region_setRangeLowerBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theLowerBound, void **out_exc) {
    invoke(setRangeLowerBound(theExtent, theDimension, theLowerBound))
}
ccall void wrap_Region_setRangeUpperBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theUpperBound, void **out_exc) {
    invoke(setRangeLowerBound(theExtent, theDimension, theUpperBound))
}


ccall RTI_ULong wrap_Region_getSpaceHandle(void *theRegion, void **out_exc) {
    invoke(getSpaceHandle())
}

ccall RTI_ULong wrap_Region_getNumberOfExtents(void *theRegion, void **out_exc) {
    invoke(getNumberOfExtents())
}

ccall RTI_ULong wrap_Region_getRangeLowerBoundNotificationLimit(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc) {
    invoke(getRangeLowerBoundNotificationLimit(theExtent, theDimension))
}

ccall RTI_ULong wrap_Region_getRangeUpperBoundNotificationLimit(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc) {
    invoke(getRangeUpperBoundNotificationLimit(theExtent, theDimension))
}

#undef invoke

///// FedTime
#define invoke(method)  wrap(return ((rti13::FedTime *)fTime)->method)

ccall void wrap_delete_FedTime(void *fTime, void **out_exc) {
    wrap(delete ((rti13::FedTime *) fTime))
}

ccall void wrap_FedTime_setZero(void *fTime, void **out_exc) {
    invoke(setZero())
}

ccall HsBool wrap_FedTime_isZero(void *fTime, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::FedTime *)fTime)->isZero();
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}

ccall void wrap_FedTime_setEpsilon(void *fTime, void **out_exc) {
    invoke(setEpsilon())
}

ccall void wrap_FedTime_setPositiveInfinity(void *fTime, void **out_exc) {
    invoke(setPositiveInfinity())
}

ccall HsBool wrap_FedTime_isPositiveInfinity(void *fTime, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::FedTime *)fTime)->isPositiveInfinity();
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}

/////   virtual FedTime& operator+= (const FedTime&)
/////   virtual FedTime& operator-= (const FedTime&)
/////   virtual Boolean operator<= (const FedTime&) const
/////   virtual Boolean operator< (const FedTime&) const
/////   virtual Boolean operator>= (const FedTime&) const
/////   virtual Boolean operator> (const FedTime&) const
/////   virtual Boolean operator== (const FedTime&) const
/////   virtual FedTime& operator= (const FedTime&)

ccall int wrap_FedTime_encodedLength(void *fTime, void **out_exc) {
    invoke(encodedLength())
}

ccall void wrap_FedTime_encode(void *fTime, char *buf, void **out_exc) {
    invoke(encode(buf))
}

ccall int wrap_FedTime_getPrintableLength(void *fTime, void **out_exc) {
    invoke(getPrintableLength())
}

ccall void wrap_FedTime_getPrintableString(void *fTime, char *buf, void **out_exc) {
    invoke(getPrintableString(buf))
}

#undef invoke

///// FedTimeFactory
ccall void *wrap_FedTimeFactory_makeZero(void **out_exc) {
    wrap(
        return rti13::FedTimeFactory::makeZero();
    )
}

// ccall void *wrap_FedTimeFactory_decode(const char *buf, void **out_exc) {
//     wrap(
//         return rti13::FedTimeFactory::decode(buf);
//     )
// }

///// EventRetractionHandle
ccall void dissect_EventRetractionHandle(void *in_erh, RTI_ULong *out_serial, RTI_ULong *out_federate) {
    *out_serial   = ((rti13::EventRetractionHandle *)in_erh)->theSerialNumber;
    *out_federate = ((rti13::EventRetractionHandle *)in_erh)->sendingFederate;
}
