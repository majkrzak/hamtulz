use hamtulz_radio::{Band, Frequency};

use crate::{adi, adx};

macro_rules! define_qso {
    ($($(#[$attr:meta])* $field:ident : $ty:ty),* $(,)?) => {
        /// A single QSO per [ADIF 3.1.7 §III.C.1.b](https://www.adif.org/317/ADIF_317.htm).
        #[derive(Debug, Clone, PartialEq)]
        pub struct Qso {
            $($(#[$attr])* pub $field: Option<$ty>),*
        }

        impl TryFrom<&adi::Record> for Qso {
            type Error = crate::Error;

            fn try_from(record: &adi::Record) -> Result<Self, Self::Error> {
                let mut qso = Self {
                    $($field: None),*
                };
                for f in &record.fields {
                    $(
                        if f.name.eq_ignore_ascii_case(stringify!($field)) {
                            qso.$field = Some(
                                f.value.parse().map_err(|_| {
                                    crate::Error::InvalidFieldValue {
                                        field: stringify!($field).to_string(),
                                        value: f.value.clone(),
                                    }
                                })?,
                            );
                        }
                    )*
                }
                Ok(qso)
            }
        }

        impl From<&Qso> for adi::Record {
            fn from(qso: &Qso) -> Self {
                let mut fields = Vec::new();
                $(
                    if let Some(value) = &qso.$field {
                        fields.push(adi::Field {
                            name: stringify!($field).to_uppercase(),
                            type_indicator: None,
                            value: value.to_string(),
                        });
                    }
                )*
                adi::Record { fields }
            }
        }

        impl TryFrom<&adx::Record> for Qso {
            type Error = crate::Error;

            fn try_from(record: &adx::Record) -> Result<Self, Self::Error> {
                let mut qso = Self {
                    $($field: None),*
                };
                for f in &record.fields {
                    $(
                        if f.name.eq_ignore_ascii_case(stringify!($field)) {
                            qso.$field = Some(
                                f.value.parse().map_err(|_| {
                                    crate::Error::InvalidFieldValue {
                                        field: stringify!($field).to_string(),
                                        value: f.value.clone(),
                                    }
                                })?,
                            );
                        }
                    )*
                }
                Ok(qso)
            }
        }

        impl From<&Qso> for adx::Record {
            fn from(qso: &Qso) -> Self {
                let mut fields = Vec::new();
                $(
                    if let Some(value) = &qso.$field {
                        fields.push(adx::Field {
                            name: stringify!($field).to_uppercase(),
                            value: value.to_string(),
                        });
                    }
                )*
                adx::Record { fields }
            }
        }
    };
}

define_qso! {
    /// the contacted station's complete mailing address: full name, street address, city, postal code, and country
    address: String,
    /// the contacted station's complete mailing address: full name, street address, city, postal code, and country
    address_intl: String,
    /// ADIF version number
    adif_ver: String,
    /// the contacted station's operator's age in years in the range 0 to 120 (inclusive)
    age: String,
    /// the height of the contacted station in meters relative to Mean Sea Level (MSL)
    altitude: String,
    /// the logging station's antenna azimuth, in degrees with a value between 0 to 360 (inclusive)
    ant_az: String,
    /// the logging station's antenna elevation, in degrees with a value between -90 to 90 (inclusive)
    ant_el: String,
    /// the signal path
    ant_path: String,
    /// the contacted station's ARRL section
    arrl_sect: String,
    /// the list of awards granted by a sponsor
    award_granted: String,
    /// the list of awards submitted to a sponsor
    award_submitted: String,
    /// the geomagnetic A index at the time of the QSO in the range 0 to 400 (inclusive)
    a_index: String,
    /// QSO Band
    band: Band,
    /// in a split frequency QSO, the logging station's receiving band
    band_rx: Band,
    /// the contacted station's callsign
    call: String,
    /// contest check (e.g. for ARRL Sweepstakes)
    check: String,
    /// contest class (e.g. for ARRL Field Day)
    class: String,
    /// the date the QSO was last uploaded to the Club Log online service
    clublog_qso_upload_date: String,
    /// the upload status of the QSO on the Club Log online service
    clublog_qso_upload_status: String,
    /// the contacted station's Secondary Administrative Subdivision (e.g. US county, JA Gun)
    cnty: String,
    /// a semicolon delimited list of Secondary Administrative Subdivision Alt codes for the contacted station
    cnty_alt: String,
    /// comment field for QSO
    comment: String,
    /// comment field for QSO
    comment_intl: String,
    /// the contacted station's Continent
    cont: String,
    /// the callsign of the individual operating the contacted station
    contacted_op: String,
    /// QSO Contest Identifier
    contest_id: String,
    /// the contacted station's DXCC entity name
    country: String,
    /// the contacted station's DXCC entity name
    country_intl: String,
    /// the contacted station's CQ Zone in the range 1 to 40 (inclusive)
    cqz: String,
    /// date and time at which the ADIF file was created
    created_timestamp: String,
    /// the list of credits sought for this QSO
    credit_submitted: String,
    /// the list of credits granted to this QSO
    credit_granted: String,
    /// the contacted station's DARC DOK (District Location Code)
    darc_dok: String,
    /// date QSL received from DCL (only valid if DCL_QSL_RCVD is Y, I, or V)
    dcl_qslrdate: String,
    /// date QSL sent to DCL (only valid if DCL_QSL_SENT is Y, Q, or I)
    dcl_qslsdate: String,
    /// DCL QSL received status
    dcl_qsl_rcvd: String,
    /// DCL QSL sent status
    dcl_qsl_sent: String,
    /// the distance between the logging station and the contacted station in kilometers
    distance: String,
    /// the contacted station's DXCC Entity Code
    dxcc: String,
    /// the contacted station's email address
    email: String,
    /// the contacted station's owner's callsign
    eq_call: String,
    /// indicates whether the QSO is known to be "Authenticity Guaranteed" by eQSL
    eqsl_ag: String,
    /// date QSL received from eQSL.cc (only valid if EQSL_QSL_RCVD is Y, I, or V)
    eqsl_qslrdate: String,
    /// date QSL sent to eQSL.cc (only valid if EQSL_QSL_SENT is Y, Q, or I)
    eqsl_qslsdate: String,
    /// eQSL.cc QSL received status
    eqsl_qsl_rcvd: String,
    /// eQSL.cc QSL sent status
    eqsl_qsl_sent: String,
    /// the contacted station's FISTS CW Club member number with a value greater than 0
    fists: String,
    /// the contacted station's FISTS CW Club Century Certificate number with a value greater than 0
    fists_cc: String,
    /// new EME "initial"
    force_init: String,
    /// QSO frequency in Megahertz
    freq: Frequency,
    /// in a split frequency QSO, the logging station's receiving frequency in Megahertz
    freq_rx: Frequency,
    /// the contacted station's Maidenhead Grid Square
    gridsquare: String,
    /// supplements GRIDSQUARE for 10 or 12 character locators
    gridsquare_ext: String,
    /// import-only: use OPERATOR instead
    guest_op: String,
    /// the date the QSO was last uploaded to the HAMLOG.EU online service
    hamlogeu_qso_upload_date: String,
    /// the upload status of the QSO on the HAMLOG.EU online service
    hamlogeu_qso_upload_status: String,
    /// the date the QSO was last uploaded to the HamQTH.com online service
    hamqth_qso_upload_date: String,
    /// the upload status of the QSO on the HamQTH.com online service
    hamqth_qso_upload_status: String,
    /// the date the QSO was last uploaded to the HRDLog.net online service
    hrdlog_qso_upload_date: String,
    /// the upload status of the QSO on the HRDLog.net online service
    hrdlog_qso_upload_status: String,
    /// the contacted station's IOTA designator, in format CC-XXX
    iota: String,
    /// the contacted station's IOTA Island Identifier
    iota_island_id: String,
    /// the contacted station's ITU zone in the range 1 to 90 (inclusive)
    ituz: String,
    /// the geomagnetic K index at the time of the QSO in the range 0 to 9 (inclusive)
    k_index: String,
    /// the contacted station's latitude
    lat: String,
    /// the contacted station's longitude
    lon: String,
    /// date QSL received from ARRL Logbook of the World (only valid if LOTW_QSL_RCVD is Y, I, or V)
    lotw_qslrdate: String,
    /// date QSL sent to ARRL Logbook of the World (only valid if LOTW_QSL_SENT is Y, Q, or I)
    lotw_qslsdate: String,
    /// ARRL Logbook of the World QSL received status
    lotw_qsl_rcvd: String,
    /// ARRL Logbook of the World QSL sent status
    lotw_qsl_sent: String,
    /// maximum length of meteor scatter bursts heard by the logging station, in seconds
    max_bursts: String,
    /// QSO Mode
    mode: String,
    /// details of the contacted station's Morse key (e.g. make, model, etc)
    morse_key_info: String,
    /// the contacted station's Morse key type (e.g. straight key, bug, etc)
    morse_key_type: String,
    /// For Meteor Scatter QSOs, the name of the meteor shower in progress
    ms_shower: String,
    /// the height of the logging station in meters relative to Mean Sea Level (MSL)
    my_altitude: String,
    /// the logging station's antenna
    my_antenna: String,
    /// the logging station's antenna
    my_antenna_intl: String,
    /// the logging station's ARRL section
    my_arrl_sect: String,
    /// the logging station's city
    my_city: String,
    /// the logging station's city
    my_city_intl: String,
    /// the logging station's county
    my_cnty: String,
    /// a semicolon delimited list of Secondary Administrative Subdivision Alt codes for the logging station
    my_cnty_alt: String,
    /// the logging station's DXCC entity name
    my_country: String,
    /// the logging station's DXCC entity name
    my_country_intl: String,
    /// the logging station's CQ Zone in the range 1 to 40 (inclusive)
    my_cq_zone: String,
    /// the logging station's DARC DOK (District Location Code)
    my_darc_dok: String,
    /// the logging station's DXCC Entity Code
    my_dxcc: String,
    /// the logging station's FISTS CW Club member number with a value greater than 0
    my_fists: String,
    /// the logging station's Maidenhead Grid Square
    my_gridsquare: String,
    /// supplements MY_GRIDSQUARE for 10 or 12 character locators
    my_gridsquare_ext: String,
    /// the logging station's IOTA designator, in format CC-XXX
    my_iota: String,
    /// the logging station's IOTA Island Identifier
    my_iota_island_id: String,
    /// the logging station's ITU zone in the range 1 to 90 (inclusive)
    my_itu_zone: String,
    /// the logging station's latitude
    my_lat: String,
    /// the logging station's longitude
    my_lon: String,
    /// details of the logging station's Morse key (e.g. make, model, etc)
    my_morse_key_info: String,
    /// the logging station's Morse key type (e.g. straight key, bug, etc)
    my_morse_key_type: String,
    /// the logging operator's name
    my_name: String,
    /// the logging operator's name
    my_name_intl: String,
    /// the logging station's postal code
    my_postal_code: String,
    /// the logging station's postal code
    my_postal_code_intl: String,
    /// a comma-delimited list of one or more of the logging station's POTA reference(s)
    my_pota_ref: String,
    /// description of the logging station's equipment
    my_rig: String,
    /// description of the logging station's equipment
    my_rig_intl: String,
    /// special interest activity or event
    my_sig: String,
    /// special interest activity or event
    my_sig_intl: String,
    /// special interest activity or event information
    my_sig_info: String,
    /// special interest activity or event information
    my_sig_info_intl: String,
    /// the logging station's International SOTA Reference
    my_sota_ref: String,
    /// the code for the logging station's Primary Administrative Subdivision (e.g. US State, JA Island, VE Province)
    my_state: String,
    /// the logging station's street
    my_street: String,
    /// the logging station's street
    my_street_intl: String,
    /// counties the contacted station may claim for the CQ Magazine USA-CA award program
    my_usaca_counties: String,
    /// grid squares the contacted station may claim for the ARRL VUCC award program
    my_vucc_grids: String,
    /// the logging station's WWFF reference
    my_wwff_ref: String,
    /// the contacted station's operator's name
    name: String,
    /// the contacted station's operator's name
    name_intl: String,
    /// QSO notes
    notes: String,
    /// QSO notes
    notes_intl: String,
    /// the number of meteor scatter bursts heard by the logging station
    nr_bursts: String,
    /// the number of meteor scatter pings heard by the logging station
    nr_pings: String,
    /// the logging operator's callsign
    operator: String,
    /// the callsign of the owner of the station used to log the contact
    owner_callsign: String,
    /// the contacted station's WPX prefix
    pfx: String,
    /// a comma-delimited list of one or more of the contacted station's POTA reference(s)
    pota_ref: String,
    /// contest precedence (e.g. for ARRL Sweepstakes)
    precedence: String,
    /// ADIF programmer's identification
    programid: String,
    /// ADIF programmer's version
    programversion: String,
    /// QSO propagation mode
    prop_mode: String,
    /// public encryption key
    public_key: String,
    /// date QSO downloaded from QRZ.COM logbook
    qrzcom_qso_download_date: String,
    /// QRZ.COM logbook QSO download status
    qrzcom_qso_download_status: String,
    /// the date the QSO was last uploaded to the QRZ.COM online service
    qrzcom_qso_upload_date: String,
    /// the upload status of the QSO on the QRZ.COM online service
    qrzcom_qso_upload_status: String,
    /// a message for the contacted station's operator to be incorporated in a paper or electronic QSL
    qslmsg: String,
    /// a message for the contacted station's operator to be incorporated in a paper or electronic QSL
    qslmsg_intl: String,
    /// a message addressed to the logging station's operator incorporated in a QSL
    qslmsg_rcvd: String,
    /// QSL received date (only valid if QSL_RCVD is Y, I, or V)
    qslrdate: String,
    /// QSL sent date (only valid if QSL_SENT is Y, Q, or I)
    qslsdate: String,
    /// QSL received status
    qsl_rcvd: String,
    /// the means by which the QSL was received or is intended to be conveyed
    qsl_rcvd_via: String,
    /// QSL sent status
    qsl_sent: String,
    /// the means by which the QSL was sent or is intended to be conveyed
    qsl_sent_via: String,
    /// the contacted station's QSL route
    qsl_via: String,
    /// indicates whether the QSO was complete from the perspective of the logging station
    qso_complete: String,
    /// date on which the QSO started
    qso_date: String,
    /// date on which the QSO ended
    qso_date_off: String,
    /// indicates whether the QSO was random or scheduled
    qso_random: String,
    /// the contacted station's city
    qth: String,
    /// the contacted station's city
    qth_intl: String,
    /// the contacted station's WAE or CQ entity contained within a DXCC entity
    region: String,
    /// description of the contacted station's equipment
    rig: String,
    /// description of the contacted station's equipment
    rig_intl: String,
    /// signal report from the contacted station
    rst_rcvd: String,
    /// signal report sent to the contacted station
    rst_sent: String,
    /// the contacted station's transmitter power in Watts
    rx_pwr: String,
    /// satellite mode - a code representing the satellite's uplink band and downlink band
    sat_mode: String,
    /// name of satellite
    sat_name: String,
    /// the solar flux at the time of the QSO in the range 0 to 300 (inclusive)
    sfi: String,
    /// the name of the contacted station's special activity or interest group
    sig: String,
    /// the name of the contacted station's special activity or interest group
    sig_intl: String,
    /// information associated with the contacted station's activity or interest group
    sig_info: String,
    /// information associated with the contacted station's activity or interest group
    sig_info_intl: String,
    /// 'Y' indicates that the contacted station's operator is now a Silent Key
    silent_key: String,
    /// the contacted station's Straight Key Century Club member information
    skcc: String,
    /// the contacted station's International SOTA Reference
    sota_ref: String,
    /// contest QSO received serial number
    srx: String,
    /// contest QSO received information
    srx_string: String,
    /// the code for the contacted station's Primary Administrative Subdivision (e.g. US State, JA Island, VE Province)
    state: String,
    /// the logging station's callsign (the callsign used over the air)
    station_callsign: String,
    /// contest QSO transmitted serial number
    stx: String,
    /// contest QSO transmitted information
    stx_string: String,
    /// QSO Submode
    submode: String,
    /// indicates that the QSO information pertains to an SWL report
    swl: String,
    /// Ten-Ten number with a value greater than 0
    ten_ten: String,
    /// HHMM or HHMMSS in UTC (QSO end time)
    time_off: String,
    /// HHMM or HHMMSS in UTC (QSO start time)
    time_on: String,
    /// the logging station's power in Watts
    tx_pwr: String,
    /// the contacted station's UKSMG member number with a value greater than 0
    uksmg: String,
    /// counties credited to the QSO for the CQ Magazine USA-CA award program
    usaca_counties: String,
    /// import-only: use STATE instead
    ve_prov: String,
    /// grid squares credited to the QSO for the ARRL VUCC award program
    vucc_grids: String,
    /// the contacted station's URL
    web: String,
    /// the contacted station's WWFF reference
    wwff_ref: String,
}
