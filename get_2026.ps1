# PowerShell script to fetch budget data from presupuestoabierto.gob.ar API

# Define the API endpoint
$uri = "https://www.presupuestoabierto.gob.ar/api/v1/credito?format=json"

# Define headers
$headers = @{
    'Authorization' = 'cbbd85c1-1986-4491-a5f6-8de8f4deb733'
    'Content-Type' = 'application/json'
}

# Define the request body
$body = @{
    columns = @(
        "impacto_presupuestario_fecha",
        "impacto_presupuestario_anio",
        "impacto_presupuestario_mes",
        "ejercicio_presupuestario",
        "sector_id",
        "sector_desc",
        "subsector_id",
        "subsector_desc",
        "caracter_id",
        "caracter_desc",
        "jurisdiccion_id",
        "jurisdiccion_desc",
        "subjurisdiccion_id",
        "subjurisdiccion_desc",
        "entidad_id",
        "entidad_desc",
        "servicio_id",
        "servicio_desc",
        "programa_id",
        "programa_desc",
        "subprograma_id",
        "subprograma_desc",
        "proyecto_id",
        "proyecto_desc",
        "actividad_id",
        "actividad_desc",
        "obra_id",
        "obra_desc",
        "finalidad_id",
        "finalidad_desc",
        "funcion_id",
        "funcion_desc",
        "inciso_id",
        "inciso_desc",
        "principal_id",
        "principal_desc",
        "parcial_id",
        "parcial_desc",
        "subparcial_id",
        "subparcial_desc",
        "clasificador_economico_8_digitos_id",
        "clasificador_economico_8_digitos_desc",
        "fuente_financiamiento_id",
        "fuente_financiamiento_desc",
        "ubicacion_geografica_id",
        "ubicacion_geografica_desc",
        "unidad_ejecutora_id",
        "unidad_ejecutora_desc",
        "prestamo_externo_id",
        "prestamo_externo_desc",
        "codigo_bapin_id",
        "codigo_bapin_desc",
        "credito_presupuestado",
        "credito_vigente",
        "credito_comprometido",
        "credito_devengado",
        "credito_pagado",
        "ultima_actualizacion_fecha"
    )
    ejercicios = @(2026)
    filters = @(
        @{
            column = "programa_id"
            value = "44"
            operator = "equal"
        },
        @{
            column = "programa_desc"
            value = "Promocion  y  Financiamiento  de  Actividades  de  Ciencia, Tecnologia e Innovacion"
            operator = "equal"
        }
    )
} | ConvertTo-Json -Depth 3

# Make the POST request and save response to file
try {
    Write-Host "Making API request (saving raw JSON to agencia\\2026.json)..." -ForegroundColor Yellow

    # Ensure the agencia directory exists before writing
    if (!(Test-Path "agencia")) {
        New-Item -ItemType Directory -Path "agencia" | Out-Null
    }

    # Save the raw server response bytes directly to agencia\2026.json
    Invoke-WebRequest -Uri $uri -Method POST -Headers $headers -Body $body -ContentType "application/json" -OutFile "agencia\\2026.json"

    Write-Host "Data successfully saved to agencia\2026.json" -ForegroundColor Green
    # Download BCRA exchange-rate Excel (com3500.xls)
    $bcraUrl = "https://www.bcra.gob.ar/pdfs/publicacionesestadisticas/com3500.xls"
    try {
        Invoke-WebRequest -Uri $bcraUrl -OutFile "com3500.xls"
        Write-Host "Downloaded com3500.xls" -ForegroundColor Green
    } catch {
        Write-Warning "Failed to download com3500.xls: $($_.Exception.Message)"
    }
    
} catch {
    Write-Error "Failed to fetch data: $($_.Exception.Message)"
    exit 1
}