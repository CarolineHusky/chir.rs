import "./style.scss";

async function getLanguageCookie(): Promise<string | undefined> {
  const Cookies = await import("js-cookie");
  return Cookies.default.get("_LANG");
}

async function getThemeCookie(): Promise<string | undefined> {
  const Cookies = await import("js-cookie");
  return Cookies.default.get("_THEME");
}

async function getTheme(): Promise<string> {
  const themeCookie = await getThemeCookie();
  if (themeCookie) {
    return themeCookie;
  } else {
    return "auto";
  }
}

function applyTheme(theme: string | undefined): void {
  if (theme === undefined || theme == "auto")
    document.documentElement.removeAttribute("data-theme");
  else document.documentElement.setAttribute("data-theme", theme);
}

function getLanguageQueryParam(): string | undefined {
  const url = new URL(window.location.href);
  let lang: string | null | undefined = url.searchParams.get("_LANG");
  if (lang === null) {
    lang = undefined;
  }
  return lang;
}

async function getLanguage(): Promise<string> {
  let queryLang = getLanguageQueryParam();
  let cookieLang = await getLanguageCookie();
  if (queryLang !== undefined) {
    await setLanguageCookie(queryLang);
    return queryLang;
  }
  if (cookieLang !== undefined) {
    return cookieLang;
  }
  return "auto";
}

async function setLanguageCookie(language: string): Promise<void> {
  const Cookies = await import("js-cookie");
  if (language !== "auto")
    Cookies.default.set("_LANG", language, {
      expires: 365,
      secure: true,
      sameSite: "Strict",
    });
  else Cookies.default.remove("_LANG");
}

async function setThemeCookie(theme: string): Promise<void> {
  const Cookies = await import("js-cookie");
  if (theme !== "auto")
    Cookies.default.set("_THEME", theme, {
      expires: 365,
      secure: true,
      sameSite: "Strict",
    });
  else Cookies.default.remove("_THEME");
}

async function updateLanguage(e: Event): Promise<void> {
  e.preventDefault();
  const lang = (e.target as HTMLSelectElement).value;
  await setLanguageCookie(lang);
  let uri = new URL(window.location.href);
  uri.searchParams.delete("_LANG");
  window.location.replace(uri);
}

async function updateTheme(e: Event): Promise<void> {
  e.preventDefault();
  const theme = (e.target as HTMLSelectElement).value;
  await setThemeCookie(theme);
  applyTheme(theme);
}

export async function main() {
  applyTheme(await getThemeCookie());
  let chosenLanguage = await getLanguage();
  let chosenTheme = await getTheme();
  let changeLanguage = document.getElementById("change-language")!!;
  let changeLanguageOptions = changeLanguage.getElementsByTagName("option")!!;
  for (let i = 0; i < changeLanguageOptions.length; i++) {
    changeLanguageOptions[i].selected =
      changeLanguageOptions[i].value == chosenLanguage;
  }
  changeLanguage.addEventListener("change", updateLanguage);
  let changeTheme = document.getElementById("change-theme")!!;
  let changeThemeOptions = changeTheme.getElementsByTagName("option")!!;
  for (let i = 0; i < changeThemeOptions.length; i++) {
    changeThemeOptions[i].selected = changeThemeOptions[i].value == chosenTheme;
  }
  changeTheme.addEventListener("change", updateTheme);
}

main();
