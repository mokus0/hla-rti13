#ifndef ___n_RTItypes_h__
#define ___n_RTItypes_h__

#include "wrap/common.h"

ccall RTI_ULong wrap_MIN_EXTENT();
ccall RTI_ULong wrap_MAX_EXTENT();

ccall void wrap_delete_AttributeHandleSet(void *ahSet, void **out_exc);
ccall void wrap_attributeHandleSet_add(void *ahSet, RTI_ULong ah, void **out_exc);
ccall void *wrap_attributeHandleSetFactory_create(RTI_ULong n, void **out_exc);

ccall void wrap_delete_ParameterHandleValuePairSet(void *pSet, void **out_exc);
ccall void wrap_parameterHandleValuePairSet_add(void *pSet, RTI_ULong handle, const char *buf, RTI_ULong len, void **out_exc);
ccall void *wrap_parameterSetFactory_create(RTI_ULong count, void **out_exc);

ccall void wrap_delete_Region(void *theRegion, void **out_exc);
ccall void wrap_setRangeLowerBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theLowerBound, void **out_exc);
ccall void wrap_setRangeUpperBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theUpperBound, void **out_exc);

#endif /* ___n_RTItypes_h__ */
