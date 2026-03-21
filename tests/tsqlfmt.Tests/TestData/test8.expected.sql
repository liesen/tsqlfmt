SELECT id,
    name,
    description
FROM items
WHERE status = 'active'
    AND (
        category_name = 'electronics_and_appliances'
        OR subcategory_description = 'home_entertainment_systems_and_accessories'
        OR manufacturer_country_of_origin = 'united_states_of_america'
    );
