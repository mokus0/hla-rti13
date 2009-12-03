#include <iostream>
#include "wrap/RTIambServices.h"

#include "wrap/rti.h"

#define invoke(method)  wrap(return ((rti13::RTIambassador *)amb)->method)

ccall void *wrap_new_RTIambassador(void **out_exc) {
    wrap(return new rti13::RTIambassador());
}

ccall void wrap_delete_RTIambassador(void *amb, void **out_exc) {
    wrap(delete ((rti13::RTIambassador *) amb));
}

////////////////////////////////////
// Federation Management Services //
////////////////////////////////////

ccall void wrap_createFederationExecution(void *amb, const char *executionName, const char *fed, void **out_exc) {
    invoke(createFederationExecution(executionName, fed))
}

ccall void wrap_destroyFederationExecution(void *amb, const char *executionName, void **out_exc) {
    invoke(destroyFederationExecution(executionName))
}

ccall RTI_ULong wrap_joinFederationExecution (void *amb,const char *yourName, const char *executionName, void *federateAmbassadorReference, void **out_exc) {
    invoke(joinFederationExecution(
            yourName, executionName, 
            (rti13::FederateAmbassadorPtr) federateAmbassadorReference))
}

ccall void wrap_resignFederationExecution(void *amb, int theAction, void **out_exc) {
    invoke(resignFederationExecution((rti13::ResignAction) theAction))
}

ccall void wrap_registerFederationSynchronizationPoint(void *amb, const char *label, const char *theTag, void **out_exc) {
    invoke(registerFederationSynchronizationPoint(label, theTag))
}

ccall void wrap_registerFederationSynchronizationPoint_with_syncSet(void *amb, const char *label, const char *theTag, void *syncSet, void **out_exc) {
    invoke(registerFederationSynchronizationPoint(label, theTag, *(rti13::FederateHandleSet *)syncSet))
}

ccall void wrap_synchronizationPointAchieved(void *amb, const char *label, void **out_exc) {
    invoke(synchronizationPointAchieved(label))
}

ccall void wrap_requestFederationSaveAtTime(void *amb, const char *label, void *theTime, void **out_exc) {
    invoke(requestFederationSave(label, *(rti13::FedTime *)theTime))
}

ccall void wrap_requestFederationSave(void *amb, const char *label, void **out_exc) {
    invoke(requestFederationSave(label))
}

ccall void wrap_federateSaveBegun(void *amb, void **out_exc) {
    invoke(federateSaveBegun())
}

ccall void wrap_federateSaveComplete(void *amb, void **out_exc) {
    invoke(federateSaveComplete())
}

ccall void wrap_federateSaveNotComplete(void *amb, void **out_exc) {
    invoke(federateSaveNotComplete())
}

ccall void wrap_requestFederationRestore(void *amb, const char *label, void **out_exc) {
    invoke(requestFederationRestore(label))
}

ccall void wrap_federateRestoreComplete(void *amb, void **out_exc) {
    invoke(federateRestoreComplete())
}

ccall void wrap_federateRestoreNotComplete(void *amb, void **out_exc) {
    invoke(federateRestoreNotComplete())
}


/////////////////////////////////////
// Declaration Management Services //
/////////////////////////////////////

ccall void wrap_publishObjectClass(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc) {
    rti13::AttributeHandleSet *ahSet = (rti13::AttributeHandleSet *) attributeList;
    invoke(publishObjectClass(theClass, *ahSet))
}

ccall void wrap_unpublishObjectClass(void *amb, RTI_ULong theClass, void **out_exc) {
    invoke(unpublishObjectClass(theClass))
}

ccall void wrap_publishInteractionClass(void *amb, RTI_ULong theClass, void **out_exc) {
    invoke(publishInteractionClass(theClass))
}

ccall void wrap_unpublishInteractionClass(void *amb, RTI_ULong theClass, void **out_exc) {
    invoke(unpublishInteractionClass(theClass))
}

ccall void wrap_subscribeObjectClassAttributes(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc)  {
    rti13::AttributeHandleSet *ahSet = (rti13::AttributeHandleSet *) attributeList;
    invoke(subscribeObjectClassAttributes(theClass, *ahSet))
}

ccall void wrap_unsubscribeObjectClass(void *amb, RTI_ULong theClass, void **out_exc)  {
    invoke(unsubscribeObjectClass(theClass))
}

ccall void wrap_subscribeInteractionClass(void *amb, RTI_ULong theClass, HsBool active, void **out_exc) {
    invoke(subscribeInteractionClass(theClass, active ? rti13::RTI_TRUE : rti13::RTI_FALSE))
}

ccall void wrap_unsubscribeInteractionClass(void *amb, RTI_ULong theClass, void **out_exc) {
    invoke(unsubscribeInteractionClass(theClass))
}

////////////////////////////////
// Object Management Services //
////////////////////////////////

ccall RTI_ULong wrap_registerObjectInstance_withName(void *amb, RTI_ULong theClass, const char *theObject, void **out_exc) {
    invoke(registerObjectInstance(theClass, theObject))
}

ccall RTI_ULong wrap_registerObjectInstance(void *amb, RTI_ULong theClass, void **out_exc) {
    invoke(registerObjectInstance(theClass))
}

ccall void wrap_updateAttributeValuesAtTime(void *amb, RTI_ULong theObject, void *theAttributes, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc) {
    wrap(
        rti13::EventRetractionHandle erh = ((rti13::RTIambassador *)amb)->updateAttributeValues(
            theObject, 
            *(rti13::AttributeHandleValuePairSet *)theAttributes, 
            *(rti13::FedTime *)theTime,
            theTag);
        *out_uniq       = erh.theSerialNumber;
        *out_fedHandle  = erh.sendingFederate;
    )
}

ccall void wrap_updateAttributeValues(void *amb, RTI_ULong theObject, void *theAttributes, const char *theTag, void **out_exc) {
    invoke(updateAttributeValues(
        theObject, 
        *(rti13::AttributeHandleValuePairSet *)theAttributes, 
        theTag))
}

ccall RTI_ULong wrap_sendInteractionAtTime(void *amb, RTI_ULong theInteraction, void *theParameters, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc) {
    rti13::ParameterHandleValuePairSet *params = (rti13::ParameterHandleValuePairSet *)theParameters;
    rti13::FedTime *time = (rti13::FedTime *) theTime;
    
    wrap(
        rti13::EventRetractionHandle handle =
            ((rti13::RTIambassador *)amb)->
                sendInteraction(theInteraction, *params, *time, theTag);
        *out_uniq       = handle.theSerialNumber;
        *out_fedHandle  = handle.sendingFederate;
    );
}

ccall void wrap_sendInteraction(void *amb, RTI_ULong theInteraction, void *theParameters, const char *theTag, void **out_exc) {
    rti13::ParameterHandleValuePairSet *params = (rti13::ParameterHandleValuePairSet *)theParameters;
    invoke(sendInteraction(theInteraction, *params, theTag))
}

ccall void wrap_deleteObjectInstanceAtTime(void *amb, RTI_ULong theObject, void *theTime, const char *theTag, RTI_ULong *out_uniq, RTI_ULong *out_fedHandle, void **out_exc) {
    wrap(
        rti13::EventRetractionHandle erh = ((rti13::RTIambassador *)amb)->deleteObjectInstance(
            theObject, 
            *(rti13::FedTime *)theTime,
            theTag);
        *out_uniq       = erh.theSerialNumber;
        *out_fedHandle  = erh.sendingFederate;
    )
}

ccall void wrap_deleteObjectInstance(void *amb, RTI_ULong theObject, const char *theTag, void **out_exc) {
    invoke(deleteObjectInstance(theObject, theTag))
}

ccall void wrap_localDeleteObjectInstance(void *amb, RTI_ULong theObject, void **out_exc) {
    invoke(localDeleteObjectInstance(theObject))
}

ccall void wrap_changeAttributeTransportationType(void *amb, RTI_ULong theObject, void *theAttributes, RTI_ULong theType, void **out_exc) {
    invoke(changeAttributeTransportationType(theObject, *(rti13::AttributeHandleSet *)theAttributes, theType))
}

ccall void wrap_changeInteractionTransportationType(void *amb, RTI_ULong theClass, RTI_ULong theType, void **out_exc) {
    invoke(changeInteractionTransportationType(theClass, theType))
}

ccall void wrap_requestObjectAttributeValueUpdate(void *amb, RTI_ULong theObject, void *attributeList, void **out_exc)  {
    rti13::AttributeHandleSet *ahSet = (rti13::AttributeHandleSet *) attributeList;
    invoke(requestClassAttributeValueUpdate(theObject, *ahSet))
}

ccall void wrap_requestClassAttributeValueUpdate(void *amb, RTI_ULong theClass, void *attributeList, void **out_exc)  {
    rti13::AttributeHandleSet *ahSet = (rti13::AttributeHandleSet *) attributeList;
    invoke(requestClassAttributeValueUpdate(theClass, *ahSet))
}

///////////////////////////////////
// Ownership Management Services //
///////////////////////////////////

ccall void wrap_unconditionalAttributeOwnershipDivestiture(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc) {
    invoke(unconditionalAttributeOwnershipDivestiture(theObject, *(rti13::AttributeHandleSet *)theAttributes))
}

ccall void wrap_negotiatedAttributeOwnershipDivestiture(void *amb, RTI_ULong theObject, void *theAttributes, char *theTag, void **out_exc) {
    invoke(negotiatedAttributeOwnershipDivestiture(theObject, *(rti13::AttributeHandleSet *)theAttributes, theTag))
}

ccall void wrap_attributeOwnershipAcquisition(void *amb, RTI_ULong theObject, void *theAttributes, char *theTag, void **out_exc) {
    invoke(attributeOwnershipAcquisition(theObject, *(rti13::AttributeHandleSet *)theAttributes, theTag))
}

ccall void wrap_attributeOwnershipAcquisitionIfAvailable(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc) {
    invoke(attributeOwnershipAcquisitionIfAvailable(theObject, *(rti13::AttributeHandleSet *)theAttributes))
}

ccall void *wrap_attributeOwnershipReleaseResponse(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc) {
    invoke(attributeOwnershipReleaseResponse(theObject, *(rti13::AttributeHandleSet *)theAttributes))
}

ccall void wrap_cancelNegotiatedAttributeOwnershipDivestiture(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc) {
    invoke(cancelNegotiatedAttributeOwnershipDivestiture(theObject, *(rti13::AttributeHandleSet *)theAttributes))
}

ccall void wrap_cancelAttributeOwnershipAcquisition(void *amb, RTI_ULong theObject, void *theAttributes, void **out_exc) {
    invoke(cancelAttributeOwnershipAcquisition(theObject, *(rti13::AttributeHandleSet *)theAttributes))
}

ccall void wrap_queryAttributeOwnership(void *amb, RTI_ULong theObject, RTI_ULong theAttribute, void **out_exc) {
    invoke(queryAttributeOwnership(theObject, theAttribute))
}

ccall HsBool wrap_isAttributeOwnedByFederate(void *amb, RTI_ULong theObject, RTI_ULong theAttribute, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::RTIambassador *)amb)->isAttributeOwnedByFederate(theObject, theAttribute);
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}


//////////////////////////////
// Time Management Services //
//////////////////////////////

ccall void wrap_enableTimeRegulation (void *amb, const void *theFederateTime, const void *theLookahead, void **out_exc) {
    std::cout << "enableTimeRegulation\n";
    invoke(enableTimeRegulation(
            * (rti13::FedTime *) theFederateTime,
            * (rti13::FedTime *) theLookahead))
}

ccall void wrap_disableTimeRegulation (void *amb, void **out_exc) {
    invoke(disableTimeRegulation())
}

ccall void wrap_enableTimeConstrained (void *amb, void **out_exc) {
    invoke(enableTimeConstrained())
}

ccall void wrap_disableTimeConstrained (void *amb, void **out_exc) {
    invoke(disableTimeConstrained())
}

ccall void wrap_timeAdvanceRequest(void *amb, void *theTime, void **out_exc) {
    const rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(timeAdvanceRequest(*t))
}

ccall void wrap_timeAdvanceRequestAvailable(void *amb, void *theTime, void **out_exc) {
    const rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(timeAdvanceRequestAvailable(*t))
}

ccall void wrap_nextEventRequest(void *amb, void *theTime, void **out_exc) {
    const rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(nextEventRequest(*t))
}

ccall void wrap_nextEventRequestAvailable(void *amb, void *theTime, void **out_exc) {
    const rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(nextEventRequestAvailable(*t))
}

ccall void wrap_flushQueueRequest(void *amb, void *theTime, void **out_exc) {
    const rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(flushQueueRequest(*t))
}

ccall void wrap_enableAsynchronousDelivery(void *amb, void **out_exc) {
    invoke(enableAsynchronousDelivery())
}

ccall void wrap_disableAsynchronousDelivery(void *amb, void **out_exc) {
    invoke(disableAsynchronousDelivery())
}

ccall void wrap_queryLBTS(void *amb, void *theTime, void **out_exc) {
    rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(queryLBTS(*t))
}

ccall void wrap_queryFederateTime(void *amb, void *theTime, void **out_exc) {
    rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(queryFederateTime(*t))
}

ccall void wrap_queryMinNextEventTime(void *amb, void *theTime, void **out_exc) {
    rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(queryMinNextEventTime(*t))
}


ccall void wrap_modifyLookahead(void *amb, void *theTime, void **out_exc) {
    const rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(modifyLookahead(*t))
}

ccall void wrap_queryLookahead(void *amb, void *theTime, void **out_exc) {
    rti13::FedTime *t = (rti13::FedTime *) theTime;
    invoke(queryLookahead(*t))
}

ccall void wrap_retract(void *amb, RTI_ULong theSerial, RTI_ULong theFederate, void **out_exc) {
    rti13::EventRetractionHandle erh;
    erh.theSerialNumber = theSerial; 
    erh.sendingFederate = theFederate;
    
    invoke(retract(erh))
}

ccall void wrap_changeAttributeOrderType(void *amb, RTI_ULong theObject, void *theAttributes, RTI_ULong theType, void **out_exc) {
    invoke(changeAttributeOrderType(theObject, *(rti13::AttributeHandleSet *)theAttributes, theType))
}

ccall void wrap_changeInteractionOrderType(void *amb, RTI_ULong theClass, RTI_ULong theType, void **out_exc) {
    invoke(changeInteractionOrderType(theClass, theType))
}

//////////////////////////////////
// Data Distribution Management //
//////////////////////////////////

ccall void *wrap_createRegion(void *amb, RTI_ULong theSpace, RTI_ULong numberOfExtents, void **out_exc) {
    invoke(createRegion(theSpace, numberOfExtents))
}

ccall void wrap_notifyAboutRegionModification(void *amb, void *theRegion, void **out_exc) {
    rti13::Region *reg = (rti13::Region *)theRegion;
    invoke(notifyAboutRegionModification(*reg))
}

ccall void wrap_deleteRegion(void *amb, void *theRegion, void **out_exc) {
    invoke(deleteRegion((rti13::Region *)theRegion))
}

ccall RTI_ULong wrap_registerObjectInstanceWithRegion(void *amb, RTI_ULong theClass, const char *theObject, RTI_ULong theAttributes[], void *theRegions[], RTI_ULong theNumberOfHandles, void **out_exc)
{
    invoke(registerObjectInstanceWithRegion(theClass, theObject, theAttributes, (rti13::Region **) theRegions, theNumberOfHandles))
}

ccall void wrap_subscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, HsBool active, void **out_exc) {
    rti13::Region *reg = (rti13::Region *)theRegion;
    invoke(subscribeInteractionClassWithRegion(theClass, *reg, active ? rti13::RTI_TRUE : rti13::RTI_FALSE))
}

ccall void wrap_unsubscribeInteractionClassWithRegion(void *amb, RTI_ULong theClass, void *theRegion, void **out_exc) {
    rti13::Region *reg = (rti13::Region *)theRegion;
    invoke(unsubscribeInteractionClassWithRegion(theClass, *reg))
}

//////////////////////////
// RTI Support Services //
//////////////////////////

ccall RTI_ULong wrap_getObjectClassHandle(void *amb, const char *theName, void **out_exc) {
    invoke(getObjectClassHandle(theName))
}

ccall RTI_ULong wrap_getAttributeHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc)  {
    invoke(getAttributeHandle(theName,whichClass))
}

ccall RTI_ULong wrap_getInteractionClassHandle(void *amb, const char *theName, void **out_exc) {
    invoke(getInteractionClassHandle(theName))
}

ccall RTI_ULong wrap_getParameterHandle(void *amb, const char *theName, RTI_ULong whichClass, void **out_exc) {
    invoke(getParameterHandle(theName, whichClass))
}

ccall RTI_ULong wrap_getRoutingSpaceHandle(void *amb, const char *theName, void **out_exc) {
    invoke(getRoutingSpaceHandle(theName))
}

ccall RTI_ULong wrap_getDimensionHandle(void *amb, const char *theName, RTI_ULong whichSpace, void **out_exc) {
    invoke(getDimensionHandle(theName, whichSpace))
}

ccall void wrap_enableAttributeRelevanceAdvisorySwitch(void *amb, void **out_exc) {
    invoke(enableAttributeRelevanceAdvisorySwitch())
}

ccall HsBool wrap_tick_minimum_maximum(void *amb, double min, double max, void **out_exc) {
    wrap(
        rti13::Boolean res = ((rti13::RTIambassador *)amb)->tick(min,max);
        return res ? HS_BOOL_TRUE : HS_BOOL_FALSE;
    )
}

