#ifdef DEBUG
#   include <iostream>
#   define dprint(str) std::cout << str << std::endl
#else
#define dprint(str)
#endif

#include "wrap/rti.h"
#include "hsFederateAmb.h"

class HsFederateAmbassador;
typedef void (*setter)(HsFederateAmbassador *, HsFunPtr *);

// this array is scanned through when constructing and destructing HsFederateAmbassador.
// all function pointer setters should be here, otherwise fields will not be properly
// initialized or finalized, and there will almost certainly be a segfault as a result.
static setter setters[] =
{   (setter) &hsfa_set_synchronizationPointRegistrationSucceeded,
    (setter) &hsfa_set_synchronizationPointRegistrationFailed,
    (setter) &hsfa_set_announceSynchronizationPoint,
    (setter) &hsfa_set_federationSynchronized,
    (setter) &hsfa_set_initiateFederateSave,
    (setter) &hsfa_set_federationSaved,
    (setter) &hsfa_set_federationNotSaved,
    (setter) &hsfa_set_requestFederationRestoreSucceeded,
    (setter) &hsfa_set_requestFederationRestoreFailed,
    (setter) &hsfa_set_federationRestoreBegun,
    (setter) &hsfa_set_initiateFederateRestore,
    (setter) &hsfa_set_federationRestored,
    (setter) &hsfa_set_federationNotRestored,
    (setter) &hsfa_set_startRegistrationForObjectClass,
    (setter) &hsfa_set_stopRegistrationForObjectClass,
    (setter) &hsfa_set_turnUpdatesOnForObjectInstance,
    (setter) &hsfa_set_turnUpdatesOffForObjectInstance,
    (setter) &hsfa_set_requestAttributeOwnershipAssumption,
    (setter) &hsfa_set_attributeOwnershipDivestitureNotification,
    (setter) &hsfa_set_attributeOwnershipAcquisitionNotification,
    (setter) &hsfa_set_attributeOwnershipUnavailable,
    (setter) &hsfa_set_requestAttributeOwnershipRelease,
    (setter) &hsfa_set_confirmAttributeOwnershipAcquisitionCancellation,
    (setter) &hsfa_set_informAttributeOwnership,
    (setter) &hsfa_set_attributeIsNotOwned,
    (setter) &hsfa_set_attributeOwnedByRTI,
    (setter) &hsfa_set_discoverObjectInstance,
    (setter) &hsfa_set_timeRegulationEnabled,
    (setter) &hsfa_set_timeConstrainedEnabled,
    (setter) &hsfa_set_timeAdvanceGrant,
    (setter) &hsfa_set_requestRetraction,
    (setter) &hsfa_set_turnInteractionsOn,
    (setter) &hsfa_set_turnInteractionsOff,
    (setter) &hsfa_set_removeObjectInstance,
    (setter) &hsfa_set_attributesInScope,
    (setter) &hsfa_set_attributesOutOfScope,
    (setter) &hsfa_set_provideAttributeValueUpdate,
    (setter) &hsfa_set_reflectAttributeValues,
    (setter) &hsfa_set_receiveInteraction,
    NULL
};

class HsFederateAmbassador : public rti13::FederateAmbassador {
public:
    FunPtrFn hs_releaseFunPtr;
    inline void releaseFun(HsFunPtr fn) {
        if (hs_releaseFunPtr && fn) hs_releaseFunPtr(fn);
    }
    
    HsFederateAmbassador(FunPtrFn releaseFunPtr) {
        hs_releaseFunPtr = NULL;
        for(int i = 0; setters[i]; i++) {
            setters[i](this, NULL);
        }
        
        hs_releaseFunPtr = releaseFunPtr;
    }
    virtual ~HsFederateAmbassador()
    throw (rti13::FederateInternalError) {
        for(int i = 0; setters[i]; i++) {
            setters[i](this, NULL);
        }
    }
    
    ////////////////////////////////////
    // Federation Management Services //
    ////////////////////////////////////
    
    ConstPtr_to_Void hsSynchronizationPointRegistrationSucceeded;
    virtual void synchronizationPointRegistrationSucceeded (
        const char *label) // supplied C4)
    throw (
        rti13::FederateInternalError) {
        if (hsSynchronizationPointRegistrationSucceeded)
            hsSynchronizationPointRegistrationSucceeded(label);
    }
    
    ConstPtr_to_Void hsSynchronizationPointRegistrationFailed;
    virtual void synchronizationPointRegistrationFailed (
        const char *label) // supplied C4)
    throw (
        rti13::FederateInternalError) {
        if (hsSynchronizationPointRegistrationFailed)
            hsSynchronizationPointRegistrationFailed(label);
    }
    
    ConstPtrX2_to_Void hsAnnounceSynchronizationPoint;
    virtual void announceSynchronizationPoint (
        const char *label, // supplied C4
        const char *tag)   // supplied C4
    throw (
        rti13::FederateInternalError) {
        if (hsAnnounceSynchronizationPoint)
            hsAnnounceSynchronizationPoint(label, tag);
    }
    
    ConstPtr_to_Void hsFederationSynchronized;
    virtual void federationSynchronized (
        const char *label) // supplied C4)
    throw (
        rti13::FederateInternalError) {
        if (hsFederationSynchronized)
            hsFederationSynchronized(label);
    }
    
    ConstPtr_to_Void hsInitiateFederateSave;
    virtual void initiateFederateSave (
        const char *label) // supplied C4
    throw (
        rti13::UnableToPerformSave,
        rti13::FederateInternalError) {
        if (hsInitiateFederateSave)
            hsInitiateFederateSave(label);
    }
    
    VoidFunc hsFederationSaved;
    virtual void federationSaved ()
    throw (
        rti13::FederateInternalError) {
        if (hsFederationSaved)
            hsFederationSaved();
    }
    
    VoidFunc hsFederationNotSaved;
    virtual void federationNotSaved ()
    throw (
        rti13::FederateInternalError) {
        if (hsFederationNotSaved)
            hsFederationNotSaved();
    }
    
    ConstPtr_to_Void hsRequestFederationRestoreSucceeded;
    virtual void requestFederationRestoreSucceeded (
        const char *label) // supplied C4
    throw (
        rti13::FederateInternalError) {
        if (hsRequestFederationRestoreSucceeded)
            hsRequestFederationRestoreSucceeded(label);
    }
    
    ConstPtrX2_to_Void hsRequestFederationRestoreFailed;
    virtual void requestFederationRestoreFailed (
        const char *label,
        const char *reason) // supplied C4
    throw (
        rti13::FederateInternalError) {
        if (hsRequestFederationRestoreFailed)
            hsRequestFederationRestoreFailed(label,reason);
    }
    
    VoidFunc hsFederationRestoreBegun;
    virtual void federationRestoreBegun ()
    throw (
        rti13::FederateInternalError) {
        if (hsFederationRestoreBegun)
            hsFederationRestoreBegun();
    }
    
    ConstPtr_to_ULong_to_Void hsInitiateFederateRestore;
    virtual void initiateFederateRestore (
        const char               *label,   // supplied C4
        rti13::FederateHandle handle)  // supplied C1
    throw (
        rti13::SpecifiedSaveLabelDoesNotExist,
        rti13::CouldNotRestore,
        rti13::FederateInternalError) {
        if (hsInitiateFederateRestore)
            hsInitiateFederateRestore(label, handle);
    }
    
    VoidFunc hsFederationRestored;
    virtual void federationRestored ()
    throw (
        rti13::FederateInternalError) {
        if (hsFederationRestored) hsFederationRestored();
    }
    
    VoidFunc hsFederationNotRestored;
    virtual void federationNotRestored ()
    throw (
        rti13::FederateInternalError) {
        if (hsFederationNotRestored) hsFederationNotRestored();
    }
    
    /////////////////////////////////////
    // Declaration Management Services //
    /////////////////////////////////////
    
    ULong_to_Void hsStartRegistrationForObjectClass;
    virtual void startRegistrationForObjectClass (
        rti13::ObjectClassHandle   theClass)      // supplied C1
    throw (
        rti13::ObjectClassNotPublished,
        rti13::FederateInternalError) {
        if (hsStartRegistrationForObjectClass)
            hsStartRegistrationForObjectClass(theClass);
    }
    
    ULong_to_Void hsStopRegistrationForObjectClass;
    virtual void stopRegistrationForObjectClass (
        rti13::ObjectClassHandle   theClass)      // supplied C1
    throw (
        rti13::ObjectClassNotPublished,
        rti13::FederateInternalError) {
        if (hsStopRegistrationForObjectClass)
            hsStopRegistrationForObjectClass(theClass);
    }
    
    ULong_to_Void hsTurnInteractionsOn;
    virtual void turnInteractionsOn (
        rti13::InteractionClassHandle theHandle) // supplied C1
    throw (
        rti13::InteractionClassNotPublished,
        rti13::FederateInternalError) {
        if (hsTurnInteractionsOn)
            hsTurnInteractionsOn(theHandle);
    }
    
    ULong_to_Void hsTurnInteractionsOff;
    virtual void turnInteractionsOff (
        rti13::InteractionClassHandle theHandle) // supplied C1
    throw (
        rti13::InteractionClassNotPublished,
        rti13::FederateInternalError) {
        if (hsTurnInteractionsOff)
            hsTurnInteractionsOff(theHandle);
    }
    
    ////////////////////////////////
    // Object Management Services //
    ////////////////////////////////
    
    ULong_to_ULong_to_ConstPtr_to_Void hsDiscoverObjectInstance;
    virtual void discoverObjectInstance (
        rti13::ObjectHandle          theObject,      // supplied C1
        rti13::ObjectClassHandle     theObjectClass, // supplied C1
        const char*                      theObjectName)  // supplied C4
    throw (
        rti13::CouldNotDiscover,
        rti13::ObjectClassNotKnown,
        rti13::FederateInternalError) {
        dprint("discoverObjectInstance: " << (void *) hsDiscoverObjectInstance);
        if (hsDiscoverObjectInstance)
            hsDiscoverObjectInstance(theObject, theObjectClass, theObjectName);
    }
    
    ULong_to_ConstPtrX3_to_ULongX2_to_Void hsReflectAttributeValues;
    virtual void reflectAttributeValues (
        rti13::ObjectHandle                 theObject,     // supplied C1
        const rti13::AttributeHandleValuePairSet& theAttributes, // supplied C4
        const rti13::FedTime&                     theTime,       // supplied C1
        const char                             *theTag,        // supplied C4
        rti13::EventRetractionHandle        theHandle)     // supplied C1
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateOwnsAttributes,
        rti13::InvalidFederationTime,
        rti13::FederateInternalError) {
        if (hsReflectAttributeValues)
            hsReflectAttributeValues(theObject, &theAttributes, &theTime, theTag, theHandle.theSerialNumber, theHandle.sendingFederate);
    }
    
    virtual void reflectAttributeValues (
        rti13::ObjectHandle                 theObject,     // supplied C1
        const rti13::AttributeHandleValuePairSet& theAttributes, // supplied C4
        const char                             *theTag)        // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateOwnsAttributes,
        rti13::FederateInternalError) {
        if (hsReflectAttributeValues)
            hsReflectAttributeValues(theObject, &theAttributes, NULL, theTag, 0, 0);
    }
    
    ULong_to_ConstPtrX3_to_ULongX2_to_Void hsReceiveInteraction;
    // 4.6
    virtual void receiveInteraction (
        rti13::InteractionClassHandle       theInteraction, // supplied C1
        const rti13::ParameterHandleValuePairSet& theParameters,  // supplied C4
        const rti13::FedTime&                     theTime,        // supplied C4
        const char                             *theTag,         // supplied C4
        rti13::EventRetractionHandle        theHandle)      // supplied C1
    throw (
        rti13::InteractionClassNotKnown,
        rti13::InteractionParameterNotKnown,
        rti13::InvalidFederationTime,
        rti13::FederateInternalError) {
        if (hsReceiveInteraction)
            hsReceiveInteraction(theInteraction, &theParameters, &theTime, theTag, theHandle.theSerialNumber, theHandle.sendingFederate);
    }
    
    virtual void receiveInteraction (
        rti13::InteractionClassHandle       theInteraction, // supplied C1
        const rti13::ParameterHandleValuePairSet& theParameters,  // supplied C4
        const char                             *theTag)         // supplied C4
    throw (
        rti13::InteractionClassNotKnown,
        rti13::InteractionParameterNotKnown,
        rti13::FederateInternalError) {
        if (hsReceiveInteraction)
            hsReceiveInteraction(theInteraction, &theParameters, NULL, theTag, 0, 0);
    }
    
    ULong_to_ConstPtrX2_to_ULongX2_to_Void hsRemoveObjectInstance;
    virtual void removeObjectInstance (
        rti13::ObjectHandle          theObject, // supplied C1
        const rti13::FedTime&              theTime,   // supplied C4
        const char                      *theTag,    // supplied C4
        rti13::EventRetractionHandle theHandle) // supplied C1
    throw (
        rti13::ObjectNotKnown,
        rti13::InvalidFederationTime,
        rti13::FederateInternalError) {
        if (hsRemoveObjectInstance)
            hsRemoveObjectInstance(theObject, &theTime, theTag, theHandle.theSerialNumber, theHandle.sendingFederate);
    }
    
    virtual void removeObjectInstance (
        rti13::ObjectHandle          theObject, // supplied C1
        const char                      *theTag)    // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::FederateInternalError) {
        if (hsRemoveObjectInstance)
            hsRemoveObjectInstance(theObject, NULL, theTag, 0, 0);
    }
    
    ULong_to_ConstPtr_to_Void hsAttributesInScope;
    virtual void attributesInScope (
        rti13::ObjectHandle        theObject,     // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateInternalError) {
        if (hsAttributesInScope) hsAttributesInScope(theObject, &theAttributes);
    }
    
    ULong_to_ConstPtr_to_Void hsAttributesOutOfScope;
    virtual void attributesOutOfScope (
        rti13::ObjectHandle        theObject,     // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateInternalError) {
        if (hsAttributesOutOfScope) hsAttributesOutOfScope(theObject, &theAttributes);
    }
    
    ULong_to_ConstPtr_to_Void hsProvideAttributeValueUpdate;
    virtual void provideAttributeValueUpdate (
        rti13::ObjectHandle        theObject,     // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeNotOwned,
        rti13::FederateInternalError) {
        if (hsProvideAttributeValueUpdate)
            hsProvideAttributeValueUpdate(theObject, &theAttributes);
    }
    
    ULong_to_ConstPtr_to_Void hsTurnUpdatesOnForObjectInstance;
    virtual void turnUpdatesOnForObjectInstance (
        rti13::ObjectHandle        theObject,     // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotOwned,
        rti13::FederateInternalError) {
        if(hsTurnUpdatesOnForObjectInstance)
            hsTurnUpdatesOnForObjectInstance(theObject, &theAttributes);
    }
    
    ULong_to_ConstPtr_to_Void hsTurnUpdatesOffForObjectInstance;
    virtual void turnUpdatesOffForObjectInstance (
        rti13::ObjectHandle        theObject,      // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotOwned,
        rti13::FederateInternalError) {
        if(hsTurnUpdatesOffForObjectInstance)
            hsTurnUpdatesOffForObjectInstance(theObject, &theAttributes);
    }
    
    ///////////////////////////////////
    // Ownership Management Services //
    ///////////////////////////////////
    
    ULong_to_ConstPtrX2_to_Void hsRequestAttributeOwnershipAssumption;
    virtual void requestAttributeOwnershipAssumption (
        rti13::ObjectHandle        theObject,         // supplied C1
        const rti13::AttributeHandleSet& offeredAttributes, // supplied C4
        const char                    *theTag)            // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeAlreadyOwned,
        rti13::AttributeNotPublished,
        rti13::FederateInternalError) {
        if (hsRequestAttributeOwnershipAssumption)
            hsRequestAttributeOwnershipAssumption(theObject, &offeredAttributes, theTag);
    }
    
    ULong_to_ConstPtr_to_Void hsAttributeOwnershipDivestitureNotification;
    virtual void attributeOwnershipDivestitureNotification (
        rti13::ObjectHandle        theObject,          // supplied C1
        const rti13::AttributeHandleSet& releasedAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeNotOwned,
        rti13::AttributeDivestitureWasNotRequested,
        rti13::FederateInternalError) {
        if (hsAttributeOwnershipDivestitureNotification)
            hsAttributeOwnershipDivestitureNotification(theObject, &releasedAttributes);
    }
    
    ULong_to_ConstPtr_to_Void hsAttributeOwnershipAcquisitionNotification;
    virtual void attributeOwnershipAcquisitionNotification (
        rti13::ObjectHandle        theObject,         // supplied C1
        const rti13::AttributeHandleSet& securedAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeAcquisitionWasNotRequested,
        rti13::AttributeAlreadyOwned,
        rti13::AttributeNotPublished,
        rti13::FederateInternalError) {
        if (hsAttributeOwnershipAcquisitionNotification)
            hsAttributeOwnershipAcquisitionNotification(theObject, &securedAttributes);
    }
    
    ULong_to_ConstPtr_to_Void hsAttributeOwnershipUnavailable;
    virtual void attributeOwnershipUnavailable (
        rti13::ObjectHandle        theObject,         // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeAlreadyOwned,
        rti13::AttributeAcquisitionWasNotRequested,
        rti13::FederateInternalError) {
        if (hsAttributeOwnershipUnavailable)
            hsAttributeOwnershipUnavailable(theObject, &theAttributes);
    }
    
    ULong_to_ConstPtrX2_to_Void hsRequestAttributeOwnershipRelease;
    virtual void requestAttributeOwnershipRelease (
        rti13::ObjectHandle        theObject,           // supplied C1
        const rti13::AttributeHandleSet& candidateAttributes, // supplied C4
        const char                    *theTag)              // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeNotOwned,
        rti13::FederateInternalError) {
        if (hsRequestAttributeOwnershipRelease)
            hsRequestAttributeOwnershipRelease(theObject, &candidateAttributes, theTag);
    }
    
    ULong_to_ConstPtr_to_Void hsConfirmAttributeOwnershipAcquisitionCancellation;
    virtual void confirmAttributeOwnershipAcquisitionCancellation (
        rti13::ObjectHandle        theObject,         // supplied C1
        const rti13::AttributeHandleSet& theAttributes) // supplied C4
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::AttributeAlreadyOwned,
        rti13::AttributeAcquisitionWasNotCanceled,
        rti13::FederateInternalError) {
        if(hsConfirmAttributeOwnershipAcquisitionCancellation)
            hsConfirmAttributeOwnershipAcquisitionCancellation(theObject, &theAttributes);
    }
    
    ULongX3_to_Void hsInformAttributeOwnership;
    virtual void informAttributeOwnership (
        rti13::ObjectHandle    theObject,    // supplied C1
        rti13::AttributeHandle theAttribute, // supplied C1
        rti13::FederateHandle  theOwner)     // supplied C1
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateInternalError) {
        if(hsInformAttributeOwnership)
            hsInformAttributeOwnership(theObject, theAttribute, theOwner);
    }
    
    ULongX2_to_Void hsAttributeIsNotOwned;
    virtual void attributeIsNotOwned (
        rti13::ObjectHandle    theObject,    // supplied C1
        rti13::AttributeHandle theAttribute) // supplied C1
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateInternalError) {
        if(hsAttributeIsNotOwned)
            hsAttributeIsNotOwned(theObject, theAttribute);
    }
    
    ULongX2_to_Void hsAttributeOwnedByRTI;
    virtual void attributeOwnedByRTI (
        rti13::ObjectHandle    theObject,    // supplied C1
        rti13::AttributeHandle theAttribute) // supplied C1
    throw (
        rti13::ObjectNotKnown,
        rti13::AttributeNotKnown,
        rti13::FederateInternalError) {
        if(hsAttributeOwnedByRTI)
            hsAttributeOwnedByRTI(theObject, theAttribute);
    }
    
    //////////////////////////////
    // Time Management Services //
    //////////////////////////////
    
    ConstPtr_to_Void hsTimeRegulationEnabled;
    virtual void timeRegulationEnabled (
        const  rti13::FedTime& theFederateTime) // supplied C4
    throw (
        rti13::InvalidFederationTime,
        rti13::EnableTimeRegulationWasNotPending,
        rti13::FederateInternalError) {
        if (hsTimeRegulationEnabled) hsTimeRegulationEnabled(&theFederateTime);
    }
    
    ConstPtr_to_Void hsTimeConstrainedEnabled;
    virtual void timeConstrainedEnabled (
        const rti13::FedTime& theFederateTime) // supplied C4
    throw (
        rti13::InvalidFederationTime,
        rti13::EnableTimeConstrainedWasNotPending,
        rti13::FederateInternalError) {
        if (hsTimeConstrainedEnabled) hsTimeConstrainedEnabled(&theFederateTime);
    }
    
    ConstPtr_to_Void hsTimeAdvanceGrant;
    virtual void timeAdvanceGrant (
        const rti13::FedTime& theTime) // supplied C4
    throw (
        rti13::InvalidFederationTime,
        rti13::TimeAdvanceWasNotInProgress,
        rti13::FederateInternalError) {
        if (hsTimeAdvanceGrant) hsTimeAdvanceGrant(&theTime);
    }
    
    ULongX2_to_Void hsRequestRetraction;
    virtual void requestRetraction (
        rti13::EventRetractionHandle theHandle) // supplied C1
    throw (
        rti13::EventNotKnown,
        rti13::FederateInternalError) {
        if(hsRequestRetraction)hsRequestRetraction(theHandle.theSerialNumber, theHandle.sendingFederate);
    }
};

ccall void *wrap_new_HsFederateAmbassador(FunPtrFn releaseFunPtr, void **out_exc) {
    wrap(return new HsFederateAmbassador(releaseFunPtr))
}

ccall void *wrap_delete_HsFederateAmbassador(void *fedAmb, void **out_exc) {
    dprint("deleting HsFederateAmbassador");
    wrap(delete (HsFederateAmbassador *) fedAmb)
}

#define setFunPtr(field)    dprint("HsFederateAmbassador: setting " << #field           \
                                    << " to " << (void *)theFunPtr);                    \
                            HsFederateAmbassador *fa = (HsFederateAmbassador *) fedAmb; \
                            if (fa->field != theFunPtr) {                               \
                                HsFunPtr tmp = (HsFunPtr) fa->field;                    \
                                fa->field = theFunPtr;                                  \
                                fa->releaseFun(tmp);                                    \
                            }

ccall void hsfa_set_synchronizationPointRegistrationSucceeded(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsSynchronizationPointRegistrationSucceeded)
}

ccall void hsfa_set_synchronizationPointRegistrationFailed(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsSynchronizationPointRegistrationFailed)
}

ccall void hsfa_set_announceSynchronizationPoint(void *fedAmb, ConstPtrX2_to_Void theFunPtr) {
    setFunPtr(hsAnnounceSynchronizationPoint)
}

ccall void hsfa_set_federationSynchronized(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsFederationSynchronized)
}

ccall void hsfa_set_initiateFederateSave(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsInitiateFederateSave)
}

ccall void hsfa_set_federationSaved(void *fedAmb, VoidFunc theFunPtr) {
    setFunPtr(hsFederationSaved)
}

ccall void hsfa_set_federationNotSaved(void *fedAmb, VoidFunc theFunPtr) {
    setFunPtr(hsFederationNotSaved)
}

ccall void hsfa_set_requestFederationRestoreSucceeded(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsRequestFederationRestoreSucceeded)
}

ccall void hsfa_set_requestFederationRestoreFailed(void *fedAmb, ConstPtrX2_to_Void theFunPtr) {
    setFunPtr(hsRequestFederationRestoreFailed)
}

ccall void hsfa_set_federationRestoreBegun(void *fedAmb, VoidFunc theFunPtr) {
    setFunPtr(hsFederationRestoreBegun)
}

ccall void hsfa_set_initiateFederateRestore(void *fedAmb, ConstPtr_to_ULong_to_Void theFunPtr) {
    setFunPtr(hsInitiateFederateRestore)
}

ccall void hsfa_set_federationRestored(void *fedAmb, VoidFunc theFunPtr) {
    setFunPtr(hsFederationRestored)
}

ccall void hsfa_set_federationNotRestored(void *fedAmb, VoidFunc theFunPtr) {
    setFunPtr(hsFederationNotRestored)
}

ccall void hsfa_set_startRegistrationForObjectClass(void *fedAmb, ULong_to_Void theFunPtr) {
    setFunPtr(hsStartRegistrationForObjectClass)
}

ccall void hsfa_set_stopRegistrationForObjectClass(void *fedAmb, ULong_to_Void theFunPtr) {
    setFunPtr(hsStopRegistrationForObjectClass)
}

ccall void hsfa_set_turnUpdatesOnForObjectInstance(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTurnUpdatesOnForObjectInstance)
}

ccall void hsfa_set_turnUpdatesOffForObjectInstance(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTurnUpdatesOffForObjectInstance)
}

ccall void hsfa_set_requestAttributeOwnershipAssumption(void *fedAmb, ULong_to_ConstPtrX2_to_Void theFunPtr) {
    setFunPtr(hsRequestAttributeOwnershipAssumption)
}

ccall void hsfa_set_attributeOwnershipDivestitureNotification(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsAttributeOwnershipDivestitureNotification)
}

ccall void hsfa_set_attributeOwnershipAcquisitionNotification(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsAttributeOwnershipAcquisitionNotification)
}

ccall void hsfa_set_attributeOwnershipUnavailable(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsAttributeOwnershipUnavailable)
}

ccall void hsfa_set_requestAttributeOwnershipRelease(void *fedAmb, ULong_to_ConstPtrX2_to_Void theFunPtr) {
    setFunPtr(hsRequestAttributeOwnershipRelease)
}

ccall void hsfa_set_confirmAttributeOwnershipAcquisitionCancellation(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsConfirmAttributeOwnershipAcquisitionCancellation)
}

ccall void hsfa_set_informAttributeOwnership(void *fedAmb, ULongX3_to_Void theFunPtr) {
    setFunPtr(hsInformAttributeOwnership)
}

ccall void hsfa_set_attributeIsNotOwned(void *fedAmb, ULongX2_to_Void theFunPtr) {
    setFunPtr(hsAttributeIsNotOwned)
}

ccall void hsfa_set_attributeOwnedByRTI(void *fedAmb, ULongX2_to_Void theFunPtr) {
    setFunPtr(hsAttributeOwnedByRTI)
}

ccall void hsfa_set_discoverObjectInstance(void *fedAmb, ULong_to_ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsDiscoverObjectInstance)
}

ccall void hsfa_set_timeRegulationEnabled(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTimeRegulationEnabled)
}

ccall void hsfa_set_timeConstrainedEnabled(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTimeConstrainedEnabled)
}

ccall void hsfa_set_timeAdvanceGrant(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTimeAdvanceGrant)
}

ccall void hsfa_set_requestRetraction(void *fedAmb, ULongX2_to_Void theFunPtr) {
    setFunPtr(hsRequestRetraction)
}

ccall void hsfa_set_turnInteractionsOn(void *fedAmb, ULong_to_Void theFunPtr) {
    setFunPtr(hsTurnInteractionsOn)
}

ccall void hsfa_set_turnInteractionsOff(void *fedAmb, ULong_to_Void theFunPtr) {
    setFunPtr(hsTurnInteractionsOff)
}

ccall void hsfa_set_removeObjectInstance(void * fedAmb, ULong_to_ConstPtrX2_to_ULongX2_to_Void theFunPtr) {
    setFunPtr(hsRemoveObjectInstance)
}

ccall void hsfa_set_attributesInScope(void * fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsAttributesInScope)
}

ccall void hsfa_set_attributesOutOfScope(void * fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsAttributesOutOfScope)
}

ccall void hsfa_set_provideAttributeValueUpdate(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsProvideAttributeValueUpdate)
}

ccall void hsfa_set_reflectAttributeValues(void *fedAmb, ULong_to_ConstPtrX3_to_ULongX2_to_Void theFunPtr) {
    setFunPtr(hsReflectAttributeValues)
}

ccall void hsfa_set_receiveInteraction(void *fedAmb, ULong_to_ConstPtrX3_to_ULongX2_to_Void theFunPtr) {
    setFunPtr(hsReceiveInteraction)
}
