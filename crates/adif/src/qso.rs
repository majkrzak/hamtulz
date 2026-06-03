use hamtulz_radio::{Band, Frequency};
use thiserror::Error;

use crate::adi::{Field, Record};

/// Error returned when a typed ADIF field cannot be parsed.
#[derive(Debug, Error)]
#[error("invalid value '{value}' for field {field}")]
pub struct QsoError {
    pub field: String,
    pub value: String,
}

macro_rules! define_qso {
    ($($field:ident : $ty:ty),* $(,)?) => {
        /// A single QSO from an ADI file.
        ///
        /// Represents one logged contact with all standard ADIF fields
        /// (excluding application-defined and user-defined fields).
        /// Every field is optional — absent fields are `None`.
        ///
        /// Per [ADIF 3.1.7](https://www.adif.org/317/ADIF_317.htm).
        #[derive(Debug, Clone, PartialEq)]
        pub struct Qso {
            $(pub $field: Option<$ty>),*
        }

        impl TryFrom<&Record> for Qso {
            type Error = QsoError;

            /// Extracts known ADIF fields from a wire-format [`Record`].
            ///
            /// Field names are matched case-insensitively.
            /// Returns [`QsoError`] on invalid input.
            fn try_from(record: &Record) -> Result<Self, Self::Error> {
                let mut qso = Self {
                    $($field: None),*
                };
                for f in &record.fields {
                    $(
                        if f.name.eq_ignore_ascii_case(stringify!($field)) {
                            qso.$field = Some(
                                f.value.parse().map_err(|_| QsoError {
                                    field: stringify!($field).to_string(),
                                    value: f.value.clone(),
                                })?,
                            );
                        }
                    )*
                }
                Ok(qso)
            }
        }

        impl From<&Qso> for Record {
            /// Converts back to a wire-format [`Record`].
            ///
            /// Only `Some` fields are emitted. Field names are uppercased.
            fn from(qso: &Qso) -> Self {
                let mut fields = Vec::new();
                $(
                    if let Some(value) = &qso.$field {
                        fields.push(Field {
                            name: stringify!($field).to_uppercase(),
                            type_indicator: None,
                            value: value.to_string(),
                        });
                    }
                )*
                Record { fields }
            }
        }
    };
}

define_qso! {
    address: String,
    address_intl: String,
    adif_ver: String,
    age: String,
    altitude: String,
    ant_az: String,
    ant_el: String,
    ant_path: String,
    arrl_sect: String,
    award_granted: String,
    award_submitted: String,
    a_index: String,
    band: Band,
    band_rx: Band,
    call: String,
    check: String,
    class: String,
    clublog_qso_upload_date: String,
    clublog_qso_upload_status: String,
    cnty: String,
    cnty_alt: String,
    comment: String,
    comment_intl: String,
    cont: String,
    contacted_op: String,
    contest_id: String,
    country: String,
    country_intl: String,
    cqz: String,
    created_timestamp: String,
    credit_submitted: String,
    credit_granted: String,
    darc_dok: String,
    dcl_qslrdate: String,
    dcl_qslsdate: String,
    dcl_qsl_rcvd: String,
    dcl_qsl_sent: String,
    distance: String,
    dxcc: String,
    email: String,
    eqsl_ag: String,
    eqsl_qslrdate: String,
    eqsl_qslsdate: String,
    eqsl_qsl_rcvd: String,
    eqsl_qsl_sent: String,
    eq_call: String,
    fists: String,
    fists_cc: String,
    force_init: String,
    freq: Frequency,
    freq_rx: Frequency,
    gridsquare: String,
    gridsquare_ext: String,
    guest_op: String,
    hamlogeu_qso_upload_date: String,
    hamlogeu_qso_upload_status: String,
    hamqth_qso_upload_date: String,
    hamqth_qso_upload_status: String,
    hrdlog_qso_upload_date: String,
    hrdlog_qso_upload_status: String,
    iota: String,
    iota_island_id: String,
    ituz: String,
    k_index: String,
    lat: String,
    lon: String,
    lotw_qslrdate: String,
    lotw_qslsdate: String,
    lotw_qsl_rcvd: String,
    lotw_qsl_sent: String,
    max_bursts: String,
    mode: String,
    morse_key_info: String,
    morse_key_type: String,
    ms_shower: String,
    my_altitude: String,
    my_antenna: String,
    my_antenna_intl: String,
    my_arrl_sect: String,
    my_city: String,
    my_city_intl: String,
    my_cnty: String,
    my_cnty_alt: String,
    my_country: String,
    my_country_intl: String,
    my_cq_zone: String,
    my_darc_dok: String,
    my_dxcc: String,
    my_fists: String,
    my_gridsquare: String,
    my_gridsquare_ext: String,
    my_iota: String,
    my_iota_island_id: String,
    my_itu_zone: String,
    my_lat: String,
    my_lon: String,
    my_morse_key_info: String,
    my_morse_key_type: String,
    my_name: String,
    my_name_intl: String,
    my_postal_code: String,
    my_postal_code_intl: String,
    my_pota_ref: String,
    my_rig: String,
    my_rig_intl: String,
    my_sig: String,
    my_sig_intl: String,
    my_sig_info: String,
    my_sig_info_intl: String,
    my_sota_ref: String,
    my_state: String,
    my_street: String,
    my_street_intl: String,
    my_usaca_counties: String,
    my_vucc_grids: String,
    my_wwff_ref: String,
    name: String,
    name_intl: String,
    notes: String,
    notes_intl: String,
    nr_bursts: String,
    nr_pings: String,
    operator: String,
    owner_callsign: String,
    pfx: String,
    pota_ref: String,
    precedence: String,
    programid: String,
    programversion: String,
    prop_mode: String,
    public_key: String,
    qrzcom_qso_download_date: String,
    qrzcom_qso_download_status: String,
    qrzcom_qso_upload_date: String,
    qrzcom_qso_upload_status: String,
    qslmsg: String,
    qslmsg_intl: String,
    qslmsg_rcvd: String,
    qslrdate: String,
    qslsdate: String,
    qsl_rcvd: String,
    qsl_rcvd_via: String,
    qsl_sent: String,
    qsl_sent_via: String,
    qsl_via: String,
    qso_complete: String,
    qso_date: String,
    qso_date_off: String,
    qso_random: String,
    qth: String,
    qth_intl: String,
    region: String,
    rig: String,
    rig_intl: String,
    rst_rcvd: String,
    rst_sent: String,
    rx_pwr: String,
    sat_mode: String,
    sat_name: String,
    sfi: String,
    sig: String,
    sig_intl: String,
    sig_info: String,
    sig_info_intl: String,
    silent_key: String,
    skcc: String,
    sota_ref: String,
    srx: String,
    srx_string: String,
    state: String,
    station_callsign: String,
    stx: String,
    stx_string: String,
    submode: String,
    swl: String,
    ten_ten: String,
    time_off: String,
    time_on: String,
    tx_pwr: String,
    uksmg: String,
    usaca_counties: String,
    ve_prov: String,
    vucc_grids: String,
    web: String,
    wwff_ref: String,
}
