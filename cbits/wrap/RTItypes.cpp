#include "wrap/RTItypes.h"

#include "wrap/rti.h"

#define invoke(method)  wrap(((rti13::AttributeHandleSet *)ahSet)->method)

ccall RTI_ULong wrap_MIN_EXTENT() { return MIN_EXTENT; }
ccall RTI_ULong wrap_MAX_EXTENT() { return MAX_EXTENT; }

ccall void *wrap_attributeHandleSetFactory_create(RTI_ULong n, void **out_exc) {
    wrap(return rti13::AttributeHandleSetFactory::create(n))
}

ccall void wrap_attributeHandleSet_add(void *ahSet, RTI_ULong ah, void **out_exc) {
    invoke(add(ah))
}

ccall void wrap_delete_AttributeHandleSet(void *ahSet, void **out_exc) {
    wrap(delete ((rti13::AttributeHandleSet *)ahSet))
}

#undef invoke
#define invoke(method)  wrap(((rti13::ParameterHandleValuePairSet *)pSet)->method)

ccall void wrap_delete_ParameterHandleValuePairSet(void *pSet, void **out_exc) {
    wrap(
        delete ((rti13::ParameterHandleValuePairSet *)pSet);
    )
}

ccall void wrap_parameterHandleValuePairSet_add(void *pSet, RTI_ULong handle, const char *buf, RTI_ULong len, void **out_exc) {
    invoke(add(handle, buf, len))
}

ccall void *wrap_parameterSetFactory_create(RTI_ULong count, void **out_exc) {
    wrap(
        return rti13::ParameterSetFactory::create(count);
    )
}

#undef invoke
#define invoke(method)  wrap(((rti13::Region *)theRegion)->method)

ccall void wrap_delete_Region(void *theRegion, void **out_exc) {
    wrap(delete ((rti13::Region *)theRegion))
}

ccall void wrap_setRangeLowerBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theLowerBound, void **out_exc) {
    invoke(setRangeLowerBound(theExtent, theDimension, theLowerBound))
}
ccall void wrap_setRangeUpperBound(void *theRegion, RTI_ULong theExtent, RTI_ULong theDimension, RTI_ULong theUpperBound, void **out_exc) {
    invoke(setRangeLowerBound(theExtent, theDimension, theUpperBound))
}
