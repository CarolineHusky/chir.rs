//! Test code
#![allow(clippy::expect_used)]

use std::fs;

use assert_json_diff::assert_json_eq;
use serde_json::Value;

const TEST_SOURCES: [&str; 96] = [
    "microformats-mixed/h-resume/change-log.html",
    "microformats-mixed/h-resume/mixedroots.html",
    "microformats-mixed/h-card/tworoots.html",
    "microformats-mixed/h-card/mixedproperties.html",
    "microformats-mixed/h-card/change-log.html",
    "microformats-mixed/h-entry/change-log.html",
    "microformats-mixed/h-entry/mixedroots.html",
    "microformats-v2/mixed/vendorprefix.html",
    "microformats-v2/mixed/ignoretemplate.html",
    "microformats-v2/mixed/vendorprefixproperty.html",
    "microformats-v2/mixed/id.html",
    "microformats-v2/mixed/change-log.html",
    "microformats-v2/rel/rel-urls.html",
    "microformats-v2/rel/varying-text-duplicate-rels.html",
    "microformats-v2/rel/duplicate-rels.html",
    "microformats-v2/rel/change-log.html",
    "microformats-v2/rel/nofollow.html",
    "microformats-v2/rel/xfn-elsewhere.html",
    "microformats-v2/rel/xfn-all.html",
    "microformats-v2/rel/license.html",
    "microformats-v2/h-recipe/minimum.html",
    "microformats-v2/h-recipe/all.html",
    "microformats-v2/h-recipe/change-log.html",
    "microformats-v2/h-product/justaname.html",
    "microformats-v2/h-product/justahyperlink.html",
    "microformats-v2/h-product/aggregate.html",
    "microformats-v2/h-product/change-log.html",
    "microformats-v2/h-product/simpleproperties.html",
    "microformats-v2/h-review/change-log.html",
    "microformats-v2/h-review/hyperlink.html",
    "microformats-v2/h-review/implieditem.html",
    "microformats-v2/h-review/item.html",
    "microformats-v2/h-review/vcard.html",
    "microformats-v2/h-review/justaname.html",
    "microformats-v2/h-review/photo.html",
    "microformats-v2/h-entry/justahyperlink.html",
    "microformats-v2/h-entry/justaname.html",
    "microformats-v2/h-entry/change-log.html",
    "microformats-v2/h-entry/encoding.html",
    "microformats-v2/h-entry/impliedname.html",
    "microformats-v2/h-entry/impliedvalue-nested.html",
    "microformats-v2/h-entry/scriptstyletags.html",
    "microformats-v2/h-entry/u-property.html",
    "microformats-v2/h-entry/summarycontent.html",
    "microformats-v2/h-entry/urlincontent.html",
    "microformats-v2/h-feed/simple.html",
    "microformats-v2/h-feed/implied-title.html",
    "microformats-v2/h-review-aggregate/change-log.html",
    "microformats-v2/h-review-aggregate/justahyperlink.html",
    "microformats-v2/h-review-aggregate/simpleproperties.html",
    "microformats-v2/h-review-aggregate/hevent.html",
    "microformats-v2/h-adr/geo.html",
    "microformats-v2/h-adr/change-log.html",
    "microformats-v2/h-adr/lettercase.html",
    "microformats-v2/h-adr/geourl.html",
    "microformats-v2/h-adr/justaname.html",
    "microformats-v2/h-adr/simpleproperties.html",
    "microformats-v2/h-resume/education.html",
    "microformats-v2/h-resume/work.html",
    "microformats-v2/h-resume/change-log.html",
    "microformats-v2/h-resume/justaname.html",
    "microformats-v2/h-resume/contact.html",
    "microformats-v2/h-resume/affiliation.html",
    "microformats-v2/h-resume/skill.html",
    "microformats-v2/h-card/justaname.html",
    "microformats-v2/h-card/baseurl.html",
    "microformats-v2/h-card/hyperlinkedphoto.html",
    "microformats-v2/h-card/impliedphoto.html",
    "microformats-v2/h-card/change-log.html",
    "microformats-v2/h-card/relativeurls.html",
    "microformats-v2/h-card/impliedname.html",
    "microformats-v2/h-card/hcard.html",
    "microformats-v2/h-card/extendeddescription.html",
    "microformats-v2/h-card/relativeurlsempty.html",
    "microformats-v2/h-card/p-property.html",
    "microformats-v2/h-card/impliedurl.html",
    "microformats-v2/h-card/justahyperlink.html",
    "microformats-v2/h-card/impliedurlempty.html",
    "microformats-v2/h-card/nested.html",
    "microformats-v2/h-card/childimplied.html",
    "microformats-v2/h-event/dates.html",
    "microformats-v2/h-event/justaname.html",
    "microformats-v2/h-event/combining.html",
    "microformats-v2/h-event/ampm.html",
    "microformats-v2/h-event/justahyperlink.html",
    "microformats-v2/h-event/dt-property.html",
    "microformats-v2/h-event/attendees.html",
    "microformats-v2/h-event/change-log.html",
    "microformats-v2/h-event/time.html",
    "microformats-v2/h-geo/altitude.html",
    "microformats-v2/h-geo/valuetitleclass.html",
    "microformats-v2/h-geo/abbrpattern.html",
    "microformats-v2/h-geo/change-log.html",
    "microformats-v2/h-geo/hidden.html",
    "microformats-v2/h-geo/justaname.html",
    "microformats-v2/h-geo/simpleproperties.html",
];

#[cfg_attr(test, mutants::skip)]
fn run_test(test_name: &str) {
    println!("Running test {test_name}");
    let test_case = fs::File::open(format!(
        "../../external/microformats/tests/tests/{test_name}"
    ))
    .expect("Open test case file");
    let result = crate::MicroformatsConverter::from_reader(
        test_case,
        "https://indieweb.org".parse().expect("Valid URL"),
    )
    .expect("successful parse")
    .to_value();

    let expected =
        fs::read_to_string(format!("./src/tests/{test_name}.json")).expect("test result file");
    let value: Value = serde_json::from_str(&expected).expect("deserialization");
    assert_json_eq!(result, value);
}

#[test]
fn run_tests() {
    for test_source in TEST_SOURCES {
        run_test(test_source);
    }
}

#[test]
fn test_to_str() {
    let string = crate::MicroformatsConverter::from_html(
        "<p class=\"h-card\">Frances Berriman</p>",
        "https://indieweb.org".parse().expect("Valid URL"),
    )
    .expect("successful parse")
    .to_str()
    .expect("valid serialization");
    println!("{string}");
    assert_eq!(
        string,
        r#"{"@context":{"@base":"https://indieweb.org/","@version":1.1,"mf":"http://microformats.org/profile#","relation":"http://purl.org/vocab/relationship/","schema":"https://schema.org/","vcard":"http://www.w3.org/2006/vcard/ns#","xhtml":"http://www.w3.org/1999/xhtml/vocab/"},"@type":"mf2:h-card","mf2:name":"Frances Berriman"}"#
    );
}
