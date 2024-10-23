ROFI_CMD="rofi -dmenu -theme-str listview{enabled:false;} -p"
LOCAL_STORAGE=~/.local/share/toggle
V2RAYA_URL="http://localhost:2017"

set_token() {
    login=$(echo "" | $ROFI_CMD "Enter login > ")
    password=$(echo "" | $ROFI_CMD "Enter password > " -theme-str 'entry {enabled: false;}')
    response=$(curl -s -X POST \
        "${V2RAYA_URL}/api/login" \
        -d "{\"username\": \"${login}\", \"password\": \"${password}\"}")
    
    code=$(echo $response | jq -r ".code")
    echo "${response}" | jq ".data.token" -r > "${LOCAL_STORAGE}/token"
}

get_status() {
    token=$1
    response=$(curl -s -X GET \
        "${V2RAYA_URL}/api/touch" \
        -H "Authorization: ${token}")
    echo $response | jq ".data.running" -r
}

toggle() {
    token=$1
    method=$2
    response=$(curl -s -X ${method} \
        "${V2RAYA_URL}/api/v2ray" \
        -H "Authorization: ${token}")
    code=$(echo $response | jq ".code" -r)
    echo $response | jq ".data.running" -r
}

if [[ ! -d "${LOCAL_STORAGE}" ]]; then
    mkdir "${LOCAL_STORAGE}"
fi

if [[ ! -e "${LOCAL_STORAGE}/token" ]]; then
    touch "${LOCAL_STORAGE}/token"
fi

TOKEN=$(cat "${LOCAL_STORAGE}/token")
if [[ -z "${TOKEN}" ]]; then
    set_token
    TOKEN=$(cat "${LOCAL_STORAGE}/token")
fi

STATUS=$(get_status $TOKEN)
if [[ $STATUS == "true" ]]; then
    NEW_STATUS=$(toggle $TOKEN DELETE)
else
    NEW_STATUS=$(toggle $TOKEN POST)
fi

notify-send v2rayA "running: ${NEW_STATUS}"
