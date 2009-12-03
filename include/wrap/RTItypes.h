#ifndef ___n_RTItypes_h__
#define ___n_RTItypes_h__

#include "wrap/common.h"

ccall const char *wrap_DEFAULT_SPACE_NAME();
ccall const char *wrap_DEFAULT_SPACE_DIMENSION_NAME();

ccall const char *wrap_RTI_VERSION();
ccall const char *wrap_RTI_INTERNAL_VERSION();

ccall RTI_ULong wrap_RTI_MAJOR_VERSION();
ccall RTI_ULong wrap_RTI_MINOR_VERSION();
ccall RTI_ULong wrap_RTI_RELEASE();

ccall RTI_ULong wrap_RTI_INTERNAL_MAJOR_VERSION();
ccall RTI_ULong wrap_RTI_INTERNAL_MINOR_VERSION();
ccall RTI_ULong wrap_RTI_INTERNAL_RELEASE();

ccall RTI_ULong wrap_MIN_EXTENT();
ccall RTI_ULong wrap_MAX_EXTENT();

///// AttributeHandleValuePairSet
ccall void      wrap_delete_AttributeHandleValuePairSet(void *ahSet, void **out_exc);

ccall RTI_ULong wrap_AttributeHandleValuePairSet_size(void *ahSet, void **out_exc);

ccall RTI_ULong wrap_AttributeHandleValuePairSet_getHandle(void *ahSet, RTI_ULong i, void **out_exc);
ccall RTI_ULong wrap_AttributeHandleValuePairSet_getValueLength(void *ahSet, RTI_ULong i, void **out_exc);
ccall void      wrap_AttributeHandleValuePairSet_getValue(void *ahSet, RTI_ULong i, char *buff, RTI_ULong *valueLength, void **out_exc);
ccall char     *wrap_AttributeHandleValuePairSet_getValuePointer(void *ahSet, RTI_ULong i, RTI_ULong *valueLength, void **out_exc);
ccall RTI_ULong wrap_AttributeHandleValuePairSet_getTransportType(void *ahSet, RTI_ULong i, void **out_exc);
ccall RTI_ULong wrap_AttributeHandleValuePairSet_getOrderType(void *ahSet, RTI_ULong i, void **out_exc);
ccall void     *wrap_AttributeHandleValuePairSet_getRegion(void *ahSet, RTI_ULong i, void **out_exc);

ccall void      wrap_AttributeHandleValuePairSet_add(void *ahSet, RTI_ULong ah, const char *buf, RTI_ULong len, void **out_exc);
ccall void      wrap_AttributeHandleValuePairSet_remove(void *ahSet, RTI_ULong ah, void **out_exc);
ccall void      wrap_AttributeHandleValuePairSet_moveFrom(void *ahSet, void *from, RTI_ULong *i, void **out_exc);

ccall void      wrap_AttributeHandleValuePairSet_empty(void *ahSet, void **out_exc);

ccall RTI_ULong wrap_AttributeHandleValuePairSet_start(void *ahSet, void **out_exc);
ccall RTI_ULong wrap_AttributeHandleValuePairSet_valid(void *ahSet, RTI_ULong i, void **out_exc);
ccall RTI_ULong wrap_AttributeHandleValuePairSet_next(void *ahSet, RTI_ULong i, void **out_exc);

///// AttributeSetFactory
ccall void     *wrap_AttributeSetFactory_create(RTI_ULong n, void **out_exc);

///// AttributeHandleSet
ccall void      wrap_delete_AttributeHandleSet(void *ahSet, void **out_exc);

ccall RTI_ULong wrap_AttributeHandleSet_size(void *ahSet, void **out_exc);

ccall RTI_ULong wrap_AttributeHandleSet_getHandle(void *ahSet, RTI_ULong i, void **out_exc);

ccall void      wrap_AttributeHandleSet_add(void *ahSet, RTI_ULong ah, void **out_exc);
ccall void      wrap_AttributeHandleSet_remove(void *ahSet, RTI_ULong ah, void **out_exc);

ccall void      wrap_AttributeHandleSet_empty(void *ahSet, void **out_exc);

ccall HsBool    wrap_AttributeHandleSet_isEmpty(void *ahSet, void **out_exc);
ccall HsBool    wrap_AttributeHandleSet_isMember(void *ahSet, RTI_ULong h, void **out_exc);

///// AttributeHandleSetFactory
ccall void     *wrap_AttributeHandleSetFactory_create(RTI_ULong n, void **out_exc);

///// FederateHandleSet
ccall void      wrap_delete_FederateHandleSet(void *fhSet, void **out_exc);

ccall RTI_ULong wrap_FederateHandleSet_size(void *fhSet, void **out_exc);

ccall RTI_ULong wrap_FederateHandleSet_getHandle(void *fhSet, RTI_ULong i, void **out_exc);

ccall void      wrap_FederateHandleSet_add(void *fhSet, RTI_ULong ah, void **out_exc);
ccall void      wrap_FederateHandleSet_remove(void *fhSet, RTI_ULong ah, void **out_exc);

ccall void      wrap_FederateHandleSet_empty(void *fhSet, void **out_exc);

ccall HsBool    wrap_FederateHandleSet_isMember(void *fhSet, RTI_ULong h, void **out_exc);

///// FederateHandleSetFactory
ccall void     *wrap_FederateHandleSetFactory_create(RTI_ULong n, void **out_exc);

///// ParameterHandleValuePairSet
ccall void wrap_delete_ParameterHandleValuePairSet(void *pSet, void **out_exc);

ccall RTI_ULong wrap_ParameterHandleValuePairSet_size(void *phvpSet, void **out_exc);

ccall RTI_ULong wrap_ParameterHandleValuePairSet_getHandle(void *phvpSet, RTI_ULong i, void **out_exc);
ccall RTI_ULong wrap_ParameterHandleValuePairSet_getValueLength(void *phvpSet, RTI_ULong i, void **out_exc);
ccall void      wrap_ParameterHandleValuePairSet_getValue(void *phvpSet, RTI_ULong i, char *buff, RTI_ULong *valueLength, void **out_exc);
ccall char     *wrap_ParameterHandleValuePairSet_getValuePointer(void *phvpSet, RTI_ULong i, RTI_ULong *valueLength, void **out_exc);
ccall RTI_ULong wrap_ParameterHandleValuePairSet_getTransportType(void *phvpSet, void **out_exc);
ccall RTI_ULong wrap_ParameterHandleValuePairSet_getOrderType(void *phvpSet, void **out_exc);
ccall void     *wrap_ParameterHandleValuePairSet_getRegion(void *phvpSet, void **out_exc);

ccall void      wrap_ParameterHandleValuePairSet_add(void *phvpSet, RTI_ULong h, const char *buf, RTI_ULong len, void **out_exc);
ccall void      wrap_ParameterHandleValuePairSet_remove(void *phvpSet, RTI_ULong h, void **out_exc);

ccall void      wrap_ParameterHandleValuePairSet_moveFrom(void *phvpSet, const void *from, RTI_ULong *i, void **out_exc);

ccall void      wrap_ParameterHandleValuePairSet_empty(void *phvpSet, void **out_exc);

ccall RTI_ULong wrap_ParameterHandleValuePairSet_start(void *phvpSet, void **out_exc);
ccall RTI_ULong wrap_ParameterHandleValuePairSet_valid(void *phvpSet, RTI_ULong i, void **out_exc);
ccall RTI_ULong wrap_ParameterHandleValuePairSet_next(void *phvpSet, RTI_ULong i, void **out_exc);

///// ParameterSetFactory
ccall void *wrap_ParameterSetFactory_create(RTI_ULong count, void **out_exc);

///// Region
ccall void wrap_delete_Region(void *theRegion, void **out_exc);

ccall RTI_ULong wrap_Region_getRangeLowerBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc);
ccall RTI_ULong wrap_Region_getRangeUpperBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc);

ccall void wrap_Region_setRangeLowerBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theLowerBound, void **out_exc);
ccall void wrap_Region_setRangeUpperBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theUpperBound, void **out_exc);

ccall RTI_ULong wrap_Region_getSpaceHandle(void *theRegion, void **out_exc);
ccall RTI_ULong wrap_Region_getNumberOfExtents(void *theRegion, void **out_exc);

ccall RTI_ULong wrap_Region_getRangeLowerBoundNotificationLimit(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc);
ccall RTI_ULong wrap_Region_getRangeUpperBoundNotificationLimit(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, void **out_exc);

///// FedTime
ccall void wrap_delete_FedTime(void *fTime, void **out_exc);

ccall void wrap_FedTime_setZero(void *fTime, void **out_exc);
ccall HsBool wrap_FedTime_isZero(void *fTime, void **out_exc);

ccall void wrap_FedTime_setEpsilon(void *fTime, void **out_exc);

ccall void wrap_FedTime_setPositiveInfinity(void *fTime, void **out_exc);
ccall HsBool wrap_FedTime_isPositiveInfinity(void *fTime, void **out_exc);

/////   virtual FedTime& operator+= (const FedTime&)
/////   virtual FedTime& operator-= (const FedTime&)
/////   virtual Boolean operator<= (const FedTime&) const
/////   virtual Boolean operator< (const FedTime&) const
/////   virtual Boolean operator>= (const FedTime&) const
/////   virtual Boolean operator> (const FedTime&) const
/////   virtual Boolean operator== (const FedTime&) const
/////   virtual FedTime& operator= (const FedTime&)

ccall int  wrap_FedTime_encodedLength(void *fTime, void **out_exc);
ccall void wrap_FedTime_encode(void *fTime, char *buf, void **out_exc);

ccall int  wrap_FedTime_getPrintableLength(void *fTime, void **out_exc);
ccall void wrap_FedTime_getPrintableString(void *fTime, char *buf, void **out_exc);

///// FedTimeFactory
ccall void *wrap_FedTimeFactory_makeZero(void **out_exc);
ccall void *wrap_FedTimeFactory_decode(const char *buf, void **out_exc);

///// EventRetractionHandle
ccall void dissect_EventRetractionHandle(void *in_erh, RTI_ULong *out_serial, RTI_ULong *out_federate);

#endif /* ___n_RTItypes_h__ */
